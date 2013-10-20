package MarpaX::Languages::SVG::Parser::Actions;

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.

use Moo;

use Scalar::Util 'refaddr';

use Set::Array;

use Types::Standard qw/Any Int Str/;

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
	is       => 'rw',
	isa      => Any,
	required => 1,
);

has param_count =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

our $myself;

our $VERSION = '1.00';

# ------------------------------------------------

sub BUILD
{
	my($self) = @_;
	$myself   = $self;

} # End of BUILD.

# ------------------------------------------------

sub boolean
{
	my($hashref, $t1) = @_;
	$t1 = lc $t1;
	$t1 = $t1 eq 'zero' ? 0 : 1;

	$myself -> param_count($myself -> param_count + 1);
	$myself -> new_item('boolean', $myself -> param_count, $t1);

	return $t1;

} # End of boolean.

# ------------------------------------------------

sub command
{
	my($hashref, $t1) = @_;

	$myself -> new_item('command', '-', $t1);
	$myself -> param_count(0);

	return $t1;

} # End of command.

# ------------------------------------------------

sub float
{
	my($hashref, $t1) = @_;

	$myself -> param_count($myself -> param_count + 1);
	$myself -> new_item('float', $myself -> param_count, $t1);

	return $t1;

} # End of float.

# ------------------------------------------------

sub integer
{
	my($hashref, $t1) = @_;

	$myself -> param_count($myself -> param_count + 1);
	$myself -> new_item('integer', $myself -> param_count, $t1);

	return $t1;

} # End of integer.

# --------------------------------------------------

sub log
{
	my($hashref, $level, $s) = @_;
	$level = 'notice' if (! defined $level);
	$s     = ''       if (! defined $s);

	$myself -> logger -> $level($s) if ($myself -> logger);

} # End of log.

# --------------------------------------------------

sub new_item
{
	my($self, $type, $name, $value) = @_;

	$myself -> item_count($myself -> item_count + 1);
	$myself -> items -> push
		({
			count => $myself -> item_count,
			name  => $name,
			type  => $type,
			value => $value,
		});

} # End of new_item.

# --------------------------------------------------

sub report
{
	my($self)   = @_;
	my($format) = '%6s  %-10s  %-20s  %s';

	$myself -> log(info => sprintf($format, 'Count', 'Type', 'Name', 'Value') );

	for my $item ($myself -> items -> print)
	{
		$myself -> log(info => sprintf($format, $$item{count}, $$item{type}, $$item{name}, $$item{value}) );
	}

} # End of report.

# ------------------------------------------------

sub string
{
	my($hashref, $t1) = @_;

	$myself -> new_item('string', '-', $t1);

	return $t1;

} # End of string.

# ------------------------------------------------

1;

=pod

=head1 NAME

C<MarpaX::Languages::SVG::Parser::Actions> - A nested SVG parser, using XML::SAX and Marpa::R2

=head1 Synopsis

See L<MarpaX::Languages::SVG::Parser/Synopsis>.

=head1 Description

Basically just utility routines for L<MarpaX::Languages::SVG::Parser>. Only used indirectly by L<Marpa::R2>.

Specifially, calls to methods are triggered by items in the input stream matching elements of the current
grammar (and Marpa does the calling).

Outputs to a stack managed by L<Set::Array>. See L</items()>.

=head1 Installation

See L<MarpaX::Languages::SVG::Parser/Installation>.

=head1 Constructor and Initialization

C<new()> is called as C<< my($actions) = MarpaX::Languages::SVG::Parser::Actions -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<MarpaX::Languages::SVG::Parser::Actions>.

Key-value pairs accepted in the parameter list (see also the corresponding methods
[e.g. L</logger([$log_object])>]):

=over 4

=item o logger => aLog::HandlerObject

By default, an object of type L<Log::Handler> is created which prints to STDOUT,
but given the default, nothing is actually printed.

Default: undef.

=back

=head1 Methods

=head2 boolean($t1)

Pushes the boolean $t1 onto the stack managed by $self -> items.

=head2 command($t1)

Pushes the command $t1 onto the stack managed by $self -> items.

=head2 float($t1)

Pushes the float $t1 onto the stack managed by $self -> items.

=head2 integer($t1)

Pushes the integer $t1 onto the stack managed by $self -> items.

=head2 item_count([$new_value])

Here, the [] indicate an optional parameter.

Get or set the counter used to populate the C<count> key in the hashref in the array of parsed tokens.

=head2 items()

Returns the instance of L<Set::Array> which manages the array of hashrefs holding the parsed tokens.

$object -> items -> print returns an array ref.

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

=head2 param_count([$new_value])

Here, the [] indicate an optional parameter.

Get or set the counter used to populate the C<name> (sic) key in the hashref in the array of parsed tokens,
in those cases that the array elements contain parameters for SVG commands.

See L<MarpaX::Languages::SVG::Parser/FAQ> for a discussion of this complex issue.

=head2 report()

Prints the stack managed by $self -> items.

=head2 string($t1)

Pushes the string $t1 onto the stack managed by $self -> items.

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
