{ pkgsFun ? import <nixpkgs>
, pkgs ? pkgsFun {}
, lib ? pkgs.lib
, toolchain
, haskellNix ? import <haskellnix> {}
, windows ? toolchain.windows {}
, fixedsFile ? ./fixeds.json
, fixeds ? lib.importJSON fixedsFile
}:

rec {
  windows_disk = { mitmproxyCert }: windows.runPackerStep {
    disk = windows.initialDisk {};
    extraMount = "work";
    extraMountOut = false;
    beforeScript = ''
      mkdir work
      ln -s ${sysmon} work/sysmon64.exe
      ln -s ${./sysmon.xml} work/sysmon.xml
      ln -s ${mitmproxyCert} work/mitmproxy-ca-cert.cer
    '';
    provisioners = [
      {
        type = "powershell";
        inline = [
          # install sysmon
          ''D:\sysmon64.exe -accepteula -i D:\sysmon.xml''
          # increase maximum log size for sysmon
          ''$log = Get-WinEvent -ListLog Microsoft-Windows-Sysmon/Operational; $log.MaximumSizeInBytes = 1gb; $log.SaveChanges()''
          # add mitmproxy cert as trusted
          ''certutil -addstore root D:\mitmproxy-ca-cert.cer''
        ];
      }
    ];
  };

  run_gateway = { mitmproxyCerts }: let
    configuration = import ./gateway.nix {
      inherit mitmproxyCerts;
    };
    inherit (pkgs.nixos configuration) config;
    inherit (config.system.build) toplevel;
  in pkgs.writeScript "run_gateway" ''
    ${pkgs.qemu_kvm}/bin/qemu-system-x86_64 \
      -name tracking_trackers_gateway \
      -cpu host -enable-kvm \
      -smp 4 \
      -m 1G \
      -virtfs local,path=/nix/store,security_model=none,mount_tag=store,readonly=on \
      -virtfs local,path=$(readlink -f data),security_model=none,mount_tag=data \
      -kernel ${toplevel}/kernel \
      -initrd ${toplevel}/initrd \
      -append "$(<${toplevel}/kernel-params) init=${toplevel}/init" \
      -device virtio-net,netdev=n0,mac=00:16:3E:11:11:01 \
      -netdev socket,id=n0,listen=127.0.0.1:8008 \
      -device virtio-net,netdev=n1,mac=00:16:3E:11:11:02 \
      -netdev user,id=n1
  '';

  run_windows = { mitmproxyCerts }: pkgs.writeScript "run_windows" ''
    ${pkgs.qemu_kvm}/bin/qemu-img create -f qcow2 -b ${windows_disk {
      mitmproxyCert = "${mitmproxyCerts}/mitmproxy-ca-cert.cer";
    }} -F qcow2 hdd.img
    ${pkgs.qemu_kvm}/bin/qemu-system-x86_64 \
      -name tracking_trackers_windows \
      -enable-kvm \
      -pidfile pid.txt \
      -smp 4,cores=4,threads=1,sockets=1 \
      -m 4G \
      -cpu qemu64,hv_relaxed,hv_spinlocks=0x1fff,hv_vapic,hv_time \
      -vga virtio \
      -rtc base=localtime \
      -drive file=hdd.img,format=qcow2,discard=unmap,detect-zeroes=unmap,index=0 \
      -usb -device usb-tablet \
      -serial none -parallel none \
      -device virtio-net,netdev=n0,mac=00:16:3E:11:12:01 \
      -netdev socket,id=n0,connect=127.0.0.1:8008
  '';

  tools = { seed }: let
    mitmproxyCerts = generateMitmproxyCerts {
      inherit seed;
    };
  in pkgs.linkFarm "tools" [
    {
      name = "run_gateway";
      path = run_gateway {
        inherit mitmproxyCerts;
      };
    }
    {
      name = "run_windows";
      path = run_windows {
        inherit mitmproxyCerts;
      };
    }
  ];

  haskellProject = (pkgsFun haskellNix.nixpkgsArgs).haskell-nix.stackProject {
    src = ./haskell;
    modules = [{
      packages.traffic-parser.components.exes.traffic-parser = {
        dontStrip = false;
      };
    }];
  };
  haskellPackages = {
    inherit (haskellProject.traffic-parser.components.exes)
      traffic-parser
    ;
  };

  generateMitmproxyCerts = { seed }: pkgs.runCommandLocal "mitmproxy_certs_${toString seed}" {} ''
    # client replay of empty file - the only (?) way to do one-off mitmproxy run
    touch empty
    ${pkgs.mitmproxy}/bin/mitmdump --set confdir=$out --no-server --client-replay empty
  '';

  sysmon = pkgs.fetchurl {
    inherit (fixeds.fetchurl."https://live.sysinternals.com/Sysmon64.exe") url sha256 name;
  };

  touch = {
    inherit sysmon;

    autoUpdateScript = toolchain.autoUpdateFixedsScript fixedsFile;
  };
}
