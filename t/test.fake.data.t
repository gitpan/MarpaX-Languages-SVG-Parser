use strict;
use warnings;

use Capture::Tiny 'capture';

use File::Basename; # For basename().

use Test::More;

# ------------------------------------------------

BEGIN
{
	use_ok('MarpaX::Languages::SVG::Parser');
	use_ok('MarpaX::Languages::SVG::Parser::Utils');
}

my($attribute);
my($result);

for my $file (MarpaX::Languages::SVG::Parser::Utils -> new -> get_files('data', 'dat') )
{
	$attribute = basename($file);
	$attribute =~ s/^(\w+)(\..+)$/$1/;

	# Could use Try::Tiny, but at home I want to see it die if it fails.

	(undef, undef, $result) = capture{system($^X, '-Ilib', 'scripts/test.file.pl', '-a', $attribute, '-i', $file)};

	ok($result == 0, "Processed $file");
}

done_testing;
