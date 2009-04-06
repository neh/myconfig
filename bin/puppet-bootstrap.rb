node nochost {
   case $puppetversion {
      "0.24.1": {
         include generic-systems
         include firewall-vmwarehost
         include users
         include util
         include monitoring
      }
      default: {
         include upgradepuppet
      }
   }
}

class upgradepuppet {
   file {
      "/etc/apt/sources.list":
         source => "puppet://puppet/files/sources.list.${lsbdistcodename}",
         ensure => present;
      "/etc/apt/trusted.gpg":
         source => "puppet://puppet/files/trusted.gpg",
         ensure => present;
      "/etc/puppet/puppetd.conf":
         ensure => absent;
      "/usr/sbin/runpuppet":
         source => "puppet:///files/runpuppet",
         ensure => present,
         owner => root, group => root, mode => 755;
      "/usr/sbin/runpuppet-noop":
         source => "puppet:///files/runpuppet-noop",
         ensure => present,
         owner => root, group => root, mode => 755;
   }
   exec {
      "/usr/bin/apt-get update":
         alias => "aptgetupdate",
         require => [ File["/etc/apt/sources.list"], File["/etc/apt/trusted.gpg"] ],
         subscribe => [ File["/etc/apt/sources.list"], File["/etc/apt/trusted.gpg"] ],
         refreshonly => true;
   }
   package {
      puppet:
         ensure => latest,
         require => Exec["aptgetupdate"];
      facter:
         ensure => latest,
         require => Exec["aptgetupdate"];
   }
   service {
      puppet:
         enable => false,
         require => Package[puppet];
   }
}