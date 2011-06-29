
use strict;
use JSON;
use Carp;

package RPCX;

# 1. no threads
# 2. can send and receive methods
# 3. method waits for its return, handling messages

sub new {
    my ($read, $write) = @_;
    my $self = bless {};

    $self->{json} = JSON->new;

    $self->{read} = $read;
    $self->{readbuf} = '';
    $self->{write} = $write;

    $self->{counter} = 1;
    $self->{responses} = {};
    $self->{methods} = {};

    return $self;
}

sub handle {
    my ($self, $current) = @_;
    while (1) {
        if ($self->{shutdown}) {
            return undef;
        }
        # get object from input
        $self->_readchunk;
        if ($self->{shutdown}) {
            return undef;
        }
        my ($obj, $rest) = eval { $self->{json}->decode_prefix($self->{readbuf}); };
        if ($@ ne "") {
            next;
        }
        $self->{readbuf} = $rest;

        # if request or notification, call the handler
        if (_has($obj, qw/id method params/)) {
            if (exists($obj->{methods}->{$obj->{method}})) {
                # TODO: call method, send results
            } else {
                # TODO: send error
            }
        } elsif (_has($obj, qw/id result error/)) {
            # if response, store the response for corresponding call
            $self->{responses}->{$obj->{id}} = $obj;
        } else {
            # error
        }
 
        # after interaction, check if the current call is responded, return
        if (exists($self->{responses}->{$current})) {
            my $res = $self->{responses}->{$current};
            delete $self->{responses}->{$current};
            # TODO: handle error
            return $res->{result};

        }

        # if notification, TODO
    }
}

sub call_method {
    my ($self, $method, @params) = @_;
    
    my $id = $self->{counter};
    $self->{counter}++;

    my $obj = { id => $id, method => $method, params => \@params };
    &{$self->{write}}($self->{json}->encode($obj));

    # call $self->handle
    return $self->handle($id);
}

sub _has {
    my ($obj, $props) = @_;
    for my $prop (@{$props}) {
        if (!exists($obj->{$prop})) {
            return 0;
        }
    }
    return 1;
}

1;
