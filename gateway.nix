{ nixpkgs, mitmproxyCerts }:
{ config, pkgs, ... }: let

  virtioFs = attrs: {
    fsType = "9p";
    options = [ "trans=virtio" "version=9p2000.L" "msize=16777216" "cache=loose" ];
  } // attrs;

  ethInternal = "ens5";
  ethExternal = "ens6";

in {
  system.stateVersion = "21.11";

  boot.loader.systemd-boot.enable = true;

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [
      "defaults"
      "size=1G"
      "mode=755"
    ];
  };
  fileSystems."/nix/store" = virtioFs {
    device = "store";
    neededForBoot = true;
  };
  fileSystems."/data" = virtioFs {
    device = "data";
    neededForBoot = true;
  };

  networking.hostName = "gateway";
  networking.hostId = "57495297";

  # perform as gateway
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
  networking.firewall.extraCommands = ''
    iptables -t nat -A POSTROUTING -o ${ethExternal} -j MASQUERADE
    iptables -A FORWARD -i ${ethInternal} -o ${ethExternal} -j ACCEPT
    iptables -A FORWARD -i ${ethExternal} -o ${ethInternal} -m state --state RELATED,ESTABLISHED -j ACCEPT
    iptables -t nat -A PREROUTING -i ${ethInternal} -p tcp --dport 80 -j REDIRECT --to-port 8080
    iptables -t nat -A PREROUTING -i ${ethInternal} -p tcp --dport 443 -j REDIRECT --to-port 8080
  '';
  networking.firewall.allowedTCPPorts = [8080];
  # static IP for internal interface
  networking.interfaces."${ethInternal}".ipv4.addresses = [
    {
      address = "10.5.5.1";
      prefixLength = 24;
    }
  ];
  # dhcp server for connected VMs
  services.dhcpd4 = {
    enable = true;
    interfaces = ["${ethInternal}"];
    extraConfig = ''
      option subnet-mask 255.255.255.0;
      option broadcast-address 10.5.5.255;
      option routers 10.5.5.1;
      option domain-name-servers 1.1.1.1;
      subnet 10.5.5.0 netmask 255.255.255.0 {
        range 10.5.5.100 10.5.5.200;
      }
    '';
  };
  # external interface gets IP from QEMU user stack DHCP
  networking.interfaces."${ethExternal}".useDHCP = true;

  # setup mitmproxy as transparent proxy
  # disable ICMP redirects
  boot.kernel.sysctl."net.ipv4.conf.all.send_redirects" = 0;

  time.timeZone = "UTC";

  nix.maxJobs = 1;
  nix.buildCores = 1;

  services.qemuGuest.enable = true;

  systemd.services.mitmproxy = {
    wants = ["network.target"];
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    environment.SSLKEYLOGFILE = "/data/sslkeylog.dat";
    serviceConfig = {
      Type = "simple";
      ExecStart = ''
        ${nixpkgs.mitmproxy}/bin/mitmdump \
          --mode transparent \
          --ssl-insecure \
          --set confdir=${mitmproxyCerts} \
          --set stream_large_bodies=1 \
          --showhost
      '';
      User = 1000;
      Group = 1000;
      Restart = "on-failure";
    };
  };

  systemd.services.tshark = {
    wants = ["network.target"];
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "simple";
      ExecStart = pkgs.writeShellScript "tshark" ''
        ${nixpkgs.wireshark-cli}/bin/tshark -i ${ethInternal} -w - > /data/traffic.pcap
      '';
    };
  };

  users.mutableUsers = false;
  users.users.gateway = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" ];
    password = "gateway";
  };
  users.groups.gateway = {
    gid = 1000;
    members = ["gateway"];
  };

  documentation.enable = false;
  programs.command-not-found.enable = false;
}
