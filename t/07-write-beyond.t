use Test::More 'tests' => 6;

my @values = (1..3);
my @lines    = map { "$_\n" } (4..6);
my @expected = map { "$_\n" } (
    "--- 1",
    "--- 2",
    "--- 3",
    "...",
    "4",
    "5",
    "6",
);

# --- Clean up in case previous tests died

my $test_file = 't/sandbox/write-beyond.yaml';

unlink $test_file if -e $test_file;

use YAML qw();

use_ok( 'IO::YAML' );

my $io = IO::YAML->new;

isa_ok( $io, 'IO::YAML' );

ok( $io->open($test_file, '>'), 'open' );
ok( -e $test_file, 'file created' );

print $io @values;
$io->terminate;
my $fh = $io->handle;
print $fh @lines;

ok( $io->close, 'close' );

my @contents = do { open(my $fh, $test_file) or die; <$fh> };

is_deeply( \@contents, \@expected, 'written contents' );

# --- Clean up for later tests

unlink $test_file if -e $test_file;

