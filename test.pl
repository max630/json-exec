use strict;
use RPCX;
# TODO: make read and write
my $c = RPCX->new;

print $c->call_method("exp", 1);
