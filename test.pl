use strict;
use RPCX;
use IPC::Open2;

my ($in, $out);
my $pid = open2($out, $in, "./TestServer");

my $c = RPCX->new(sub { my $buf; sysread *{$in}, $buf, 10240; return $buf; }, sub { syswrite *{$out}, shift; });

print $c->call_method("exp", 1);
