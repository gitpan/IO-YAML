# Note: this file has been initially generated by Module::Build::Convert 0.30

use strict;
use warnings;

use Module::Build;

my $build = Module::Build->new
  (
   module_name => 'IO-YAML',
   dist_author => 'Paul Hoffman (nkuitse AT cpan DOT org)',
   ,
   dist_version_from => 'lib/IO/YAML.pm',
   requires => {
                 'Errno' => 0,
                 'Fcntl' => 0,
                 'File::Copy' => 0,
                 'IO::File' => 0,
                 'IO::Scalar' => 0,
                 'Symbol' => 0,
                 'YAML' => 0,
               },
   license => 'unknown',
   create_readme => 1,
   create_makefile_pl => 'traditional',
  );
  
$build->create_build_script;
