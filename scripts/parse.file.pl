#!/usr/bin/env perl

use strict;
use utf8;
use warnings;
use open qw(:std :utf8); # Undeclared streams in UTF-8.

use Getopt::Long;

use MarpaX::Languages::SVG::Parser;

use Pod::Usage;

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
	\%option,
	'encoding=s',
	'help',
	'input_file_name=s',
	'maxlevel=s',
	'minlevel=s',
	'output_file_name=s',
) )
{
	pod2usage(1) if ($option{'help'});

	exit MarpaX::Languages::SVG::Parser -> new(%option) -> run;
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

parse.file.pl - Parsing any SVG file

=head1 SYNOPSIS

test.file.pl [options]

	Options:
	-encoding aString
	-help
	-input_file_name anSVGFileName
	-maxlevel aString
	-minlevel aString
	-output_file_name aCSVFileName

All switches can be reduced to a single letter.

Exit value: 0.

=head1 OPTIONS

=over 4

=item o -encoding aString

String takes values such as 'utf-8', and the code converts this into '<:encoding(utf-8)'.

This option is rarely needed for input, but may be for output.

Default: ''.

=item o -help

Print help and exit.

=item o -input_file_name anSVGFileName

The name of an SVG file to process.

See data/*.svg for some samples.

This option is mandatory.

Default: ''.

=item o -maxlevel logOption1

This option affects Log::Handler.

See the Log::handler docs.

Default: 'notice'.

=item o -minlevel logOption2

This option affects Log::Handler.

See the Log::handler docs.

Default: 'error'.

No lower levels are used.

=item o -output_file_name aCSVFileName

The name of a CSV file to write, of parsed tokens.

See also the -encoding option.

By default, nothing is written.

Default: ''.

=back

=cut
