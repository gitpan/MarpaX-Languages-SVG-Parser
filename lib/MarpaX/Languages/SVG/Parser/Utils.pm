package MarpaX::Languages::SVG::Parser::Utils;

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.

use Config;

use Date::Simple;

use File::Basename; # For basename().
use File::Slurp;    # For read_dir().
use File::Spec;

use MarpaX::Languages::SVG::Parser::Config;

use Moo;

use Types::Standard qw/HashRef/;

use Text::Xslate 'mark_raw';

has config =>
(
	default  => sub{return MarpaX::Languages::SVG::Parser::Config -> new -> config},
	is       => 'rw',
	isa      => HashRef,
	required => 0,
);

our $VERSION = '1.00';

# ------------------------------------------------

sub generate_demo
{
	my($self) = @_;

	# Generate html/*.svg.

	`$^X -Ilib scripts/bnf2graph.pl`;

	my($basename);
	my(%data_file);
	my($image);

	for my $file (MarpaX::Languages::SVG::Parser::Utils -> new -> get_files('data', 'bnf') )
	{
		$basename             = basename($file);
		$image                = $basename =~ s/bnf$/svg/r;
		$data_file{$basename} =
		{
			bnf   => $file,
			image => $image,
		};
	}

	my($config)    = $self -> config;
	my($templater) = Text::Xslate -> new
	(
		input_layer => '',
		path        => $$config{template_path},
	);
	my($count) = 0;
	my($index) = $templater -> render
	(
	'marpax.languages.svg.parser.tx',
	{
		default_css => "$$config{css_url}/default.css",
		data        =>
			[
			map
			{
				{
					bnf    => mark_raw($data_file{$_}{bnf}),
					count  => ++$count,
					image  => $data_file{$_}{image},
				};
			} sort keys %data_file
			],
		environment     => $self -> generate_demo_environment,
		fancy_table_css => "$$config{css_url}/fancy.table.css",
		version         => $VERSION,
	}
	);
	my($file_name) = File::Spec -> catfile('html', 'index.html');

	open(OUT, '>', $file_name);
	print OUT $index;
	close OUT;

	print "Wrote $file_name\n";

	# Return 0 for success and 1 for failure.

	return 0;

} # End of generate_demo.

# ------------------------------------------------

sub generate_demo_environment
{
	my($self) = @_;

	my(@environment);

	# mark_raw() is needed because of the HTML tag <a>.

	push @environment,
	{left => 'Author', right => mark_raw(qq|<a href="http://savage.net.au/">Ron Savage</a>|)},
	{left => 'Date',   right => Date::Simple -> today},
	{left => 'OS',     right => 'Debian V 6'},
	{left => 'Perl',   right => $Config{version} };

	return \@environment;

} # End of generate_demo_environment.

# ------------------------------------------------

sub get_files
{
	my($self, $dir_name, $type) = @_;

	return sort grep{/$type$/} read_dir($dir_name, {prefix => 1});

} # End of get_files.

# ------------------------------------------------

1;

=pod

=head1 NAME

C<MarpaX::Languages::SVG::Parser::Utils> - A nested SVG parser, using XML::SAX and Marpa::R2

=head1 Synopsis

See L<MarpaX::Languages::SVG::Parser/Synopsis>.

=head1 Description

Utility routines for L<MarpaX::Languages::SVG::Parser>.

Only used by scripts/*.pl and t/*.t.

=head1 Installation

See L<MarpaX::Languages::SVG::Parser/Installation>.

=head1 Constructor and Initialization

See L<MarpaX::Languages::SVG::Parser/Constructor and Initialization>.

=head1 Methods

=head2 generate_demo_environment()

Returns a hashref of OS, etc, values.

Keys are C<left> and C<right>, to suit C<htdocs/assets/templates/marpax/languages/svg/parser/fancy.table.tx>.

C<*.tx> files are used by L<Text::Xslate>.

Called by L</generate_demo_index()>.

=head2 generate_demo_index()

Calls L<MarpaX::Languages::SVG::Parser::Utils/get_files($dir_name, $type)> and L</generate_demo_environment()>.

Writes C<html/index.html>.

See scripts/generate.demo.pl.

=head2 get_files($dir_name, $type)

Returns an array of full file names from the $dir_name directory, whose file names end in /$type$/.

See scripts/*.pl and t/*.t.

=head2 new()

This method is auto-generated by L<Moo>.

=head1 Author

L<MarpaX::Languages::SVG::Parser> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2013.

Home page: L<http://savage.net.au/>.

=head1 Copyright

Australian copyright (c) 2013, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License 2.0, a copy of which is available at:
	http://www.opensource.org/licenses/index.html

=cut
