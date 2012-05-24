use strict;
use RPCX;
use IPC::Open2;

my ($in, $out);
my $pid = open2($in, $out, "./TestServer");

print "a\n";

my $c = RPCX->new(sub { my $buf; sysread *{$in}, $buf, 10240; return $buf; },
		  sub { print "wi0\n"; my $res = syswrite *{$out}, shift; print "wi1\n"; return $res; });

print "b\n";
print $c->call_method("exp", 1);
print "c\n";
