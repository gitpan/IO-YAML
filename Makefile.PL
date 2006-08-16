use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'         => 'IO-YAML',
    'AUTHOR'       => 'Paul Hoffman (nkuitse AT cpan DOT org)',
    'VERSION_FROM' => 'lib/IO/YAML.pm',
    'PREREQ_PM'    => {
        'YAML'       => 0,
        'IO::File'   => 0,
        'Errno'      => 0,
        'Fcntl'      => 0,
        'Symbol'     => 0,
        'File::Copy' => 0,
        'IO::Scalar' => 0,
    },
);

