package MarpaX::Languages::SVG::Parser::SAXHandler;

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.

use Data::Section::Simple 'get_data_section';

use Marpa::R2;

use MarpaX::Languages::SVG::Parser::Actions;

use Moo;

use Set::Array;

use Types::Standard qw/Any Int Str/;

extends 'XML::SAX::Base';

has actions =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any, # 'MarpaX::Languages::SVG::Parser::Actions'.
	required => 0,
);

has grammar =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any, # 'Marpa::R2::Scanless::G'.
	required => 0,
);

has item_count =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

has items =>
(
	default  => sub{return Set::Array -> new},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has logger =>
(
	default  => sub{return undef},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has recce =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any, # 'Marpa::R2::Scanless::R'.
	required => 0,
);

has text =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has text_stack =>
(
	default  => sub{return Set::Array -> new},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

our $VERSION = '1.00';

# -----------------------------------------------

sub characters
{
	my($self, $characters) = @_;

	$self -> text($self -> text . $$characters{Data});

}	# End of characters.

# --------------------------------------------------

sub encode_booleans
{
	my($self, $str) = @_;
	my(@str)        = split(/([A-Za-z])/, $str);
	my($inside_arc) = 0;

	my(@a);
	my($j);
	my($s, @s);

	while ($#str >= 0)
	{
		$s = shift @str;

		# If it's a elliptical arc, encode the 4th and 5th parameters from (0, 1) to (zero, one).

		if ($s =~ /\s*[Aa]\s*/)
		{
			push @s, $s;

			$s = shift @str;
			$s =~ s/^\s+//;
			$s =~ tr/ //c;
			@a = split(/[\s,]/, $s);

			for $j (3 .. 4)
			{
				$a[$j] =~ s/^0$/ zero /g;
				$a[$j] =~ s/^1$/ one /g;
			}

			$s = join(' ', @a);
		}

		push @s, $s;
	}

	$str = join('', @s);

	return $str;

} # End of encode_booleans.

# -----------------------------------------------

sub end_element
{
	my($self, $element) = @_;
	my($tag)            = $$element{Name};

	$self -> new_item('content', $tag, $self -> text);
	$self -> new_item('tag', $tag, 'close');
	$self -> text($self -> text_stack -> pop -> print);

}	# End of end_element.

# --------------------------------------------------

sub init_marpa
{
	my($self, %args) = @_;

	# This line is vital since it re-initializes these attributes:
	# o item_count.
	# o items.

	$self -> actions
	(
		MarpaX::Languages::SVG::Parser::Actions -> new(logger => $self -> logger)
	);

	$self -> grammar
	(
		Marpa::R2::Scanless::G -> new({source => \get_data_section("$args{attribute}.bnf")})
	);

	$self -> recce
	(
		Marpa::R2::Scanless::R -> new
		({
			grammar           => $self -> grammar,
			semantics_package => 'MarpaX::Languages::SVG::Parser::Actions',
		})
	);

} # End of init_marpa.

# --------------------------------------------------

sub log
{
	my($self, $level, $s) = @_;
	$level = 'notice' if (! defined $level);
	$s     = ''       if (! defined $s);

	$self -> logger -> $level($s) if ($self -> logger);

} # End of log.

# --------------------------------------------------

sub new_item
{
	my($self, $type, $name, $value) = @_;

	$self -> item_count($self -> item_count + 1);
	$self -> items -> push
		({
			count => $self -> item_count,
			name  => $name,
			type  => $type,
			value => $value,
		});

} # End of new_item.

# ------------------------------------------------

sub reorder_items
{
	my($self)     = @_;
	my(@items)    = @{$self -> actions -> items -> print};
	my($previous) = 0;

	# A (path) d's 'Mm' command is stacked as the 3 items: 1, 2 and m,
	# because of the way Marpa triggers callbacks. Reorder to m, 1, 2.
	# This logic must be applied to a range of commands, not just 'Mm'.

	my($i, $item);
	my($j);

	for $i (0 .. $#items)
	{
		$item = $items[$i];

		if ($$item{type} eq 'command')
		{
			$self -> new_item($$item{type}, $$item{name}, $$item{value});

			for $j ($previous .. ($i - 1) )
			{
				$item = $items[$j];

				$self -> new_item($$item{type}, $$item{name}, $$item{value});
			}

			$previous = $i + 1;
		}
	}

	# If no commands were found, the items are just numbers, so transfer them.

	if ($previous == 0)
	{
		for $i (0 .. $#items)
		{
			$item = $items[$i];

			$self -> new_item($$item{type}, $$item{name}, $$item{value});
		}
	}

} # End of reorder_items.

# -----------------------------------------------

sub run_marpa
{
	my($self, %args) = @_;

	# The 2 flags in the (path) d's 'Aa' parameter list are Booleans.
	# Here they are converted into 'zero' and 'one' to hide them from the
	# code looking for numbers. Later, their values are restored.

	my($value) = ( ($args{attribute} eq 'd') && ($args{value} =~ /[Aa]/) )
		? $self -> encode_booleans($args{value})
		: $args{value};

	$self -> log(debug => "Parsing: $value");
	$self -> init_marpa(%args);
	$self -> recce -> read(\$value);

	my($result) = $self -> recce -> value;

	die "Marpa's parse failed\n" if (! defined $result);

	$self -> reorder_items;

} # End of run_marpa.

# -----------------------------------------------

sub start_element
{
	my($self, $element) = @_;
	my($tag)            = $$element{Name};
	my(%special)        =
	(
		d                   => 1,
		points              => 1,
		preserveAspectRatio => 1,
		transform           => 1,
		viewBox             => 1,
	);

	# Stack the text parsed so far for the previous tag,
	# and start accumulating the text for the current one.

	$self -> text_stack -> push($self -> text);
	$self -> text('');
	$self -> new_item('tag', $tag, 'open');

	my($value);

	for my $attribute (sort %{$$element{Attributes} })
	{
		$value = $$element{Attributes}{$attribute};

		next if (! defined $value);

		$attribute =~ s/^{}//;
		$value     = $$value{Value};

		if ($special{$attribute})
		{
			$self -> new_item('raw', $attribute, $value);
			$self -> run_marpa(attribute => $attribute, value => $value);
		}
		else
		{
			$self -> new_item('attribute', $attribute, $value);
		}
	}

}	# End of start_element.

# -----------------------------------------------

1;

=pod

=head1 NAME

C<MarpaX::Languages::SVG::Parser::SAXHandler> - A nested SVG parser, using XML::SAX and Marpa::R2

=head1 Synopsis

See L<MarpaX::Languages::SVG::Parser/Synopsis>.

=head1 Description

Basically just utility routines for L<MarpaX::Languages::SVG::Parser>. Only used indirectly by
L<XML::SAX|XML::SAX::Base>.

Specifically, parses a SVG file, and also runs L<Marpa::R2> to parse the attribute value of some tag/attribute
combinations. Each such attribute value has its own Marpa-style BNF.

Outputs to a stack managed by L<Set::Array>. See L</items()>.

=head1 Installation

See L<MarpaX::Languages::SVG::Parser/Installation>.

=head1 Constructor and Initialization

C<new()> is called as C<< my($handler) = MarpaX::Languages::SVG::Parser::SAXHandler -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<MarpaX::Languages::SVG::Parser::SAXHandler>.

Key-value pairs accepted in the parameter list (see also the corresponding methods
[e.g. L</logger([$log_object])>]):

=over 4

=item o logger => aLog::HandlerObject

By default, an object of type L<Log::Handler> is created which prints to STDOUT,
but given the default, nothing is actually printed.

Default: undef.

=back

=head1 Methods

=head2 actions([$action_object])

Here, the [] indicate an optional parameter.

Get or set the action object.

It is always an instance of L<MarpaX::Languages::SVG::Parser::Actions>.

=head2 characters($characters)

A callback used to accumulate character text within XML tags.

See L</text($str)>.

=head2 encode_booleans($str)

Replaces certain instances of 0 and 1 within $str with 'zero' and 'one' repectively.

This is to stop the integer parser detecting them. Later, the original digits are restored.

Returns $str.

=head2 end_element($element)

A callback used to log the closing of a tag.

=head2 grammar([$grammar_object])

Here, the [] indicate an optional parameter.

Get or set the grammar object.

It is always an instance of L<Marpa::R2::Scanless::G>.

=head2 init_marpa(%args)

Initialize Marpa using the BNF named with the 'attribute' key in %args, which must be one of the attributes' names
handled specially.

Called by L</run_marpa(%args)>.

=head2 item_count([$new_value])

Here, the [] indicate an optional parameter.

Get or set the counter used to populate the C<count> key in the hashref in the array of parsed tokens.

=head2 items()

Returns the instance of L<Set::Array> which manages the array of hashrefs holding the parsed tokens.

Note: C<< $object -> items -> print >> returns an array ref.

See L<MarpaX::Languages::SVG::Parser/Synopsis> for sample code.

See also L</new_item($type, $name, $value)>.

=head2 log($level, $s)

Calls $self -> logger -> log($level => $s) if ($self -> logger).

=head2 logger([$log_object])

Here, the [] indicate an optional parameter.

Get or set the log object.

$log_object must be a L<Log::Handler>-compatible object.

To disable logging, just set logger to the empty string.

Note: C<logger> is a parameter to new().

=head2 new()

This method is auto-generated by L<Moo>.

=head2 new_item($type, $name, $value)

Pushes another hashref onto the stack managed by $self -> items.

=head2 recce([$recognizer_object])

Here, the [] indicate an optional parameter.

Get or set the recognizer object.

It is always an instance of L<Marpa::R2::Scanless::R>.

=head2 reorder_items()

A (path) d's 'Mm' command is stacked as the 3 items: 1, 2 and m,
because of the way Marpa triggers callbacks. Reorder to m, 1, 2.

This logic must be applied to a range of commands.

=head2 run_marpa(%args)

Run's the instance of Marpa created in the call to L</init_marpa(%args)>.

%args is a hash of these (key => value) pairs:

=over 4

=item o attibute => $string

See the docs for L</init_marpa(%args)> above for valid values.

=item o value => $string

The string to be parsed, using the grammar named with (attribute => $string).

=back

=head2 start_element($element)

A callback used to log the opening of a tag.

It also checks the names of all the tags attributes, and if there is one which is being treated specially (which is the
whole point of this distro), it loads the appropriate Marpa BNF and parses the value of the attribute.

The result of this Marpa-based parse is a set of items pushed onto the stack managed by L</items()>.

=head2 text($str)

Accumulates the text belonging to the most recently opened tag.

See L</characters($characters)>.

=head2 test_stack([$text])

Manages a stack of text-per-tag, since the text within a tag can be split when the tag contains nested tags.
With a stack, the inner tags can all have their own text.

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

__DATA__

@@ d.bnf

# This grammar is for a path's 'd' attribute.

:default			::= action => [values]

# G1 stuff.

:start 				::= d

d					::= move_to draw_to_commands

move_to				::=
move_to				::= Mm moveto_arguments				action => command

moveto_arguments	::= coordinate_pair

draw_to_commands	::= draw_to_command*

draw_to_command		::= close_path
						| line_to
						| horizontal_line_to
						| vertical_line_to
						| curve_to
						| smooth_curve_to
						| quadratic_bezier_curve_to
						| smooth_quadratic_bezier_curve_to
						| elliptical_arc

close_path			::= Zz								action => command

line_to				::= Ll line_to_arguments			action => command

line_to_arguments	::= coordinate_pair
						| coordinate_pair line_to_arguments

horizontal_line_to	::= Hh coordinate_list				action => command

vertical_line_to	::= Vv coordinate_list				action => command

curve_to			::= Cc curve_to_arguments			action => command

curve_to_arguments	::= coordinate_triple
						| coordinate_triple curve_to_arguments

smooth_curve_to		::= Ss line_to_arguments			action => command

quadratic_bezier_curve_to			::= Qq line_to_arguments	action => command

smooth_quadratic_bezier_curve_to	::= Tt line_to_arguments	action => command

elliptical_arc		::= Aa coordinate_pair coordinate flag flag coordinate_pair	action => command

coordinate_triple	::= coordinate coordinate coordinate

coordinate_pair		::= coordinate coordinate

coordinate_list		::= coordinate+

coordinate			::= float							action => float
						| integer						action => integer

flag				::= boolean							action => boolean

# G0 stuff.

Aa					~ [Aa]

boolean				~ 'zero'
boolean				~ 'one'

Cc					~ [Cc]

digit				~ [0-9]
digit_any			~ digit*
digit_many			~ digit+

E					~ [Ee] sign_maybe digit_many
E_maybe				~ E
E_maybe				~

:lexeme				~ integer
integer				~ sign_maybe non_zero digit_any
						| zero

:lexeme				~ float
float				~ sign_maybe digit_many E
						| sign_maybe digit_any '.' digit_many E_maybe
						| sign_maybe digit_many '.' E_maybe
						| sign_maybe non_zero digit_any

Hh					~ [Hh]

Ll					~ [Ll]

Mm					~ [Mm]

non_zero			~ [1-9]

Qq					~ [Qq]

sign_maybe			~ [+-]
sign_maybe			~

Ss					~ [Ss]

Tt					~ [Tt]

Vv					~ [Vv]

zero				~ '0'

Zz					~ [Zz]

# Boilerplate.
# \x{09} => \t. \x{0A} => \n. \x{0D} => \r. \x{20} => \s.

:discard			~ comma
comma				~ ','

:discard			~ whitespace
whitespace			~ [\s]+

###################################
@@ points.bnf

# This grammar is for both the polygon 'points' and polyline 'points' attributes.

:default			::= action => [values]

# G1 stuff.

:start 				::= coordinate_pairs

coordinate_pairs	::= coordinate_pair
						| coordinate_pair coordinate_pairs

coordinate_pair		::= coordinate coordinate

coordinate			::= float							action => float
						| integer						action => integer

# G0 stuff.

digit				~ [0-9]
digit_any			~ digit*
digit_many			~ digit+

E					~ [Ee] sign_maybe digit_many
E_maybe				~ E
E_maybe				~

:lexeme				~ integer
integer				~ sign_maybe non_zero digit_any
						| zero

:lexeme				~ float
float				~ sign_maybe digit_many E
						| sign_maybe digit_any '.' digit_many E_maybe
						| sign_maybe digit_many '.' E_maybe
						| sign_maybe non_zero digit_any

non_zero			~ [1-9]

sign_maybe			~ [+-]
sign_maybe			~

zero				~ '0'

# Boilerplate.
# \x{09} => \t. \x{0A} => \n. \x{0D} => \r. \x{20} => \s.

:discard			~ comma
comma				~ ','

:discard			~ whitespace
whitespace			~ [\s]+

###################################
@@ preserveAspectRatio.bnf

# This grammar is for 'preserveAspectRatio' attribute of various tags.

:default	::= action => [values]

# G1 stuff.

:start		::= strings

strings		::= string
				| string string
				| string string string

string		::= letter			action => string

# G0 stuff.

letter		~ [A-Za-z]+

# Boilerplate.
# \x{09} => \t. \x{0A} => \n. \x{0D} => \r. \x{20} => \s.

:discard	~ whitespace
whitespace	~ [\s]+

###################################
@@ transform.bnf

# This grammar is for 'transform' attribute of various tags.

:default			::= action => [values]

# G1 stuff.

:start 				::= transforms

transforms			::= transform
						| transform transforms

transform			::= matrix
						| translate
						| scale
						| rotate
						| skewX
						| skewY

matrix				::= 'matrix' '('
						number number number number number number
						')'								action => command

translate			::= 'translate' '(' number_set ')'	action => command

number_set			::= number
						| number number

scale				::= 'scale' '(' number_set ')'		action => command

rotate				::= 'rotate' '(' rotate_set ')'		action => command

rotate_set			::= number
						| number number

skewX				::= 'skewX' '(' number ')'			action => command

skewY				::= 'skewY' '(' number ')'			action => command

number				::= float							action => float
						| integer						action => integer

# G0 stuff.

digit				~ [0-9]
digit_any			~ digit*
digit_many			~ digit+

E					~ [Ee] sign_maybe digit_many
E_maybe				~ E
E_maybe				~

:lexeme				~ integer
integer				~ sign_maybe non_zero digit_any
						| zero

:lexeme				~ float
float				~ sign_maybe digit_many E
						| sign_maybe digit_any '.' digit_many E_maybe
						| sign_maybe digit_many '.' E_maybe
						| sign_maybe non_zero digit_any

non_zero			~ [1-9]

sign_maybe			~ [+-]
sign_maybe			~

zero				~ '0'

# Boilerplate.
# \x{09} => \t. \x{0A} => \n. \x{0D} => \r. \x{20} => \s.
:discard			~ comma
comma				~ ','

:discard			~ whitespace
whitespace			~ [\s]+

###################################
@@ viewBox.bnf

# This grammar is for the 'viewBox' attribute.

:default			::= action => [values]

# G1 stuff.

:start 				::= coordinate_pairs

coordinate_pairs    ::= coordinate_pair coordinate_pair

coordinate_pair		::= coordinate coordinate

coordinate			::= float							action => float
						| integer						action => integer

# G0 stuff.

digit				~ [0-9]
digit_any			~ digit*
digit_many			~ digit+

E					~ [Ee] sign_maybe digit_many
E_maybe				~ E
E_maybe				~

:lexeme				~ integer
integer				~ sign_maybe non_zero digit_any
						| zero

:lexeme				~ float
float				~ sign_maybe digit_many E
						| sign_maybe digit_any '.' digit_many E_maybe
						| sign_maybe digit_many '.' E_maybe
						| sign_maybe non_zero digit_any

non_zero			~ [1-9]

sign_maybe			~ [+-]
sign_maybe			~

zero				~ '0'

# Boilerplate.
# \x{09} => \t. \x{0A} => \n. \x{0D} => \r. \x{20} => \s.

:discard			~ comma
comma				~ ','

:discard			~ whitespace
whitespace			~ [\s]+
