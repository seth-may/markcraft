use strict;
use warnings;
use feature qw(say signatures);
use Scalar::Util qw(blessed);
use Carp;

# --- Moose-like OOP ---
package Observable {
    sub new ($class, %args) {
        bless { listeners => {}, state => $args{initial} // {} }, $class;
    }
    
    sub on ($self, $event, $callback) {
        push @{$self->{listeners}{$event}}, $callback;
        return sub { $self->{listeners}{$event} = [grep { $_ != $callback } @{$self->{listeners}{$event}}] };
    }
    
    sub emit ($self, $event, @data) {
        for my $cb (@{$self->{listeners}{$event} // []}) {
            $cb->(@data);
        }
    }
}

# --- Functional Pipeline ---
sub pipeline { my @fns = @_; sub { my $val = shift; $val = $_->($val) for @fns; $val } }
sub pmap (&@) { my $fn = shift; map { $fn->($_) } @_ }
sub pfilter (&@) { my $fn = shift; grep { $fn->($_) } @_ }

my $transform = pipeline(
    sub { [map { $_ * 2 } @{$_[0]}] },
    sub { [grep { $_ > 10 } @{$_[0]}] },
    sub { [sort { $b <=> $a } @{$_[0]}] },
);

# --- Regex Engine Demo ---
sub tokenize ($text) {
    my @tokens;
    while ($text =~ m{
        (?<number>  \d+(?:\.\d+)?     ) |
        (?<string>  "[^"]*"             ) |
        (?<ident>   [a-zA-Z_]\w*       ) |
        (?<op>      [+\-*/=<>!]+       ) |
        (?<paren>   [(){}\[\]]        ) |
        \s+
    }gx) {
        for my $type (qw(number string ident op paren)) {
            push @tokens, { type => $type, value => $+{$type} } if defined $+{$type};
        }
    }
    return \@tokens;
}
