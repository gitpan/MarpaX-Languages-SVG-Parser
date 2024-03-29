#!/usr/bin/env perl

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.
use open      qw(:std :utf8);    # Undeclared streams in UTF-8.

use Getopt::Long;

use MarpaX::Languages::SVG::Parser::Utils;

use Pod::Usage;

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
	\%option,
	'help',
) )
{
	pod2usage(1) if ($option{'help'});

	exit MarpaX::Languages::SVG::Parser::Utils -> new(%option) -> generate_demo;
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

generate.demo.pl - Generate MarpaX::Languages::SVG::Parser's html/*.svg and html/index.html.

=head1 SYNOPSIS

generate.demo.pl [options]

	Options:
	-help

Exit value: 0 for success, 1 for failure. Die upon error.

=head1 OPTIONS

=over 4

=item o -help

Print help and exit.

=back

=cut
