#!/usr/bin/env perl

use strict;
use warnings;

use Capture::Tiny 'capture';

use File::Basename; # For basename().

use Getopt::Long;

use MarpaX::Languages::SVG::Parser::Utils;

use Pod::Usage;

# -----------------------------------------------

sub process
{
	my(%arg) = @_;

	my($basename);
	my(@params);
	my($result);
	my($stdout, $stderr);

	for my $file (MarpaX::Languages::SVG::Parser::Utils -> new -> get_files('data', 'dat') )
	{
		$basename = basename($file);
		@params   = ();

		next if ($basename !~ /^$arg{attribute}/);

		push @params, '-Ilib', 'scripts/test.file.pl';
		push @params, '-a', $arg{attribute}, '-i', $file;
		push @params, '-enc', $arg{encoding} if ($arg{encoding});
		push @params, '-max', $arg{maxlevel} if ($arg{maxlevel});
		push @params, '-min', $arg{minlevel} if ($arg{minlevel});

		($stdout, $stderr, $result) = capture{system($^X, @params)};

		if ($stderr)
		{
			print $stderr;
		}
		else
		{
			print $stdout;
		}

		print '-' x 50, "\n";
	}

} # End of process.

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
	\%option,
	'attribute=s',
	'encoding=s',
	'help',
	'maxlevel=s',
	'minlevel=s',
) )
{
	pod2usage(1) if ($option{'help'});

	die "You must specify a value for the 'attribute'\n" if (! $option{attribute});

	process(%option);
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

test.fileset.pl - Test parsing a set of artificial files

=head1 SYNOPSIS

test.file.pl [options]

	Options:
	-attribute aString
	-encoding aString
	-help
	-maxlevel aString
	-minlevel aString

All switches can be reduced to a single letter.

Exit value: 0.

=head1 OPTIONS

=over 4

=item o -attribute aString

Specifies the type of BNF to process:

=over 4

=item o d

This is for a path's 'd' attribute.

=item o points

Use this for a polygon's and a polyline's points.

=item o preserveAspectRatio

Various tags can have a 'preserveAspectRatio' attribute.

=item o transform

Various tags can have a 'transform' attribute.

=item o viewBox

Various tags can have a 'viewBox' attribute.

=back

Default: 'd'.

=item o -encoding aString

$string takes values such as 'utf-8', and the code converts this into '<:encoding(utf-8)'.

This option is rarely needed.

Default: ''.

=item o -help

Print help and exit.

=item o -maxlevel logOption1

This option affects Log::Handler.

See the Log::handler docs.

Default: 'notice'.

=item o -minlevel logOption2

This option affects Log::Handler.

See the Log::handler docs.

Default: 'error'.

No lower levels are used.

=back

=cut
