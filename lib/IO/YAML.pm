package IO::YAML;

use YAML qw();
use IO::File;
use Errno;
use Fcntl;
use Symbol;

use vars qw($VERSION $AUTOLOAD);

$VERSION = '0.02';

sub new {
    my ($cls, @args) = @_;
    if (scalar(@args) % 2) {
        # --- Odd number of args
        my $r = ref($args[0]);
        if ($r eq '') {
            unshift @args, 'path';
        } elsif (UNIVERSAL::isa($r, 'GLOB')) {
            unshift @args, 'handle';
        } else {
            die "Odd argument can't be interpreted";
        }
    }
    my %args = (
        'auto_load' => 0,
        @args,
        'buffer'    => '',
    );
    my $self = bless Symbol::gensym(), $cls;
    foreach (keys %args) {
        *$self->{$_} = $args{$_};
    }
    return $self->init;
}

sub path { scalar @_ > 1 ? *{$_[0]}->{'path'} = $_[1] : *{$_[0]}->{'path'} }
sub mode { scalar @_ > 1 ? *{$_[0]}->{'mode'} = $_[1] : *{$_[0]}->{'mode'} }
sub auto_load { scalar @_ > 1 ? *{$_[0]}->{'auto_load'} = $_[1] : *{$_[0]}->{'auto_load'} }

sub auto_close { scalar @_ > 1 ? *{$_[0]}->{'auto_close'} = $_[1] : *{$_[0]}->{'auto_close'} }
sub buffer { scalar @_ > 1 ? *{$_[0]}->{'buffer'} = $_[1] : *{$_[0]}->{'buffer'} }
sub handle { scalar @_ > 1 ? *{$_[0]}->{'handle'} = $_[1] : *{$_[0]}->{'handle'} }

sub open {
    my ($self, $path, $mode) = @_;
    
    my $fh = $self->handle;
    if (defined $path and defined $fh) {
        # --- Reopen a different file
        $self->close;
        undef $fh;
    }
    
    if (defined $fh) {
        # Default is to read it
        $mode = '<' unless defined $mode;
    } else {
        
        $path ||= $self->path;
        
        unless (defined $path) {
            # $! = "No such file or directory";
            if (exists &Errno::ENOENT) {
                $! = &Errno::ENOENT;
            } else {
                CORE::open(gensym, undef);
            }
            return;
        }
        
        $fh = IO::File->new;
        $self->handle($fh);
        
        ($path, $mode) = $self->normalize_path_and_mode($path, $mode || $self->mode);
        $self->path($path);
        $self->mode($mode);
        
        die "Invalid file mode '$mode'; valid modes are < > >> r w a"
            unless $mode =~ /^<|>|>>$/;
        
        unless ($fh->open($path, $mode)) {
            $self->handle(undef);
            unlink $path
                if -e $path and $mode eq '>';
            return;
        }
        
        $self->auto_close(1);
        
    }
    
    $! = 0;
    return $fh;
    
}

sub close {
    my ($self) = @_;
#   return unless $self->auto_close;
    my $fh = $self->handle;
    if (defined $fh) {
        fh_close($fh);
        $self->handle(undef);
    }
    $self->mode(undef);
    undef *$self->{$_} for qw(mode);
    return $self;
}

sub print {
    my $self = shift;
    my $fh = $self->handle || $self->open || die "Can't open: $!";
    print $fh (YAML::Dump($_)) or die $!
        foreach @_;
    return 1;
}

sub getline {
    my ($self) = @_;
    my $fh = $self->handle || $self->open;
    my $buffer = $self->buffer;
    $buffer = <$fh> if $buffer eq '';
    my $lookahead = <$fh>;
    if (defined $lookahead) {
        until ($lookahead =~ /^(?:---|\.\.\.)/) {
            $buffer .= $lookahead;
            $lookahead = <$fh>;
            last unless defined $lookahead;
        }
    }
    my $retval = $self->auto_load ? YAML::Load($buffer) : $buffer;
    $self->buffer(defined $lookahead ? $lookahead : '');
    return $retval;
}

sub getlines {
    my ($self) = @_;
    my $fh = $self->handle || $self->open;
    my @lines = <$fh>;
    return YAML::Load(join('', @lines));
}

sub next {
    goto &getlines if wantarray;
    goto &getline;
}

sub seek {
    my ($self, $pos, $whence) = @_;
    die "Arbitrary seeks not allowed"
        unless $pos == 0;
    my $fh = $self->handle || $self->open;
    fh_seek($fh, $pos, $whence)
        or die "Couldn't seek: $!";
}

sub tell {
    my ($self) = @_;
    my $fh = $self->handle || $self->open;
    my $pos = fh_tell($fh);
    return unless $! eq '';
    return $pos;
}

sub truncate {
    my ($self, $length) = @_;
    die "Arbitrary truncates not allowed"
        unless $length == 0
        or $length == $self->tell;
    my $fh = $self->handle || $self->open;
    fh_truncate($fh, $length);
    return $! ne '';
}

sub eof {
    my ($self) = @_;
    my $fh = $self->handle || $self->open;
    fh_eof($fh);
}

sub DESTROY {
    my ($self) = @_;
    $self->close if $self->handle;
    unless ( $^V and $^V lt '5.8.0' ) {
        untie *$self if tied *$self;
    }
}

sub AUTOLOAD {
    my $self = shift;
    my $fh = $self->handle;
    (my $method = $AUTOLOAD) =~ s/.*:://;
    my $f = UNIVERSAL::can($fh, $method);
    die "Unknown method '$method' called"
        unless defined $f;
    unshift @_, $fh;
    goto &$f;
}

# --- Private methods

sub normalize_path_and_mode {
    my ($self, $path, $mode) = @_;
    if ($path =~ s/^(<|>|>>)\s*//) {
        $mode = $1;
    }
    return ($path, '<') unless defined $mode;
    my %mode_norm = qw(
        <   <
        >   >
        >>  >>
        r   <
        w   >
        a   >>
    );
    $mode = $mode_norm{$mode}
        or die "Unknown mode: '$mode'";
    return ($path, $mode);
}

sub init {
    my ($self) = @_;
    $self->auto_close(0);
    my $path = $self->path;
    my $fh   = $self->handle;
    if ($fh) {
        # --- Nothing to do
    } elsif (defined $path) {
        $self->open($path, $self->mode);
    } else {
        # --- Nothing to do
    }
    $self->tie; # unless $self->dont_tie;
    return $self;
}

# --- Tie interface

sub tie {
    my ($self) = @_;
    tie *$self, $self; 
    return $self;
}

sub TIEHANDLE() {
    return $_[0] if ref $_[0];
    my $class = shift;
    my $self = bless Symbol::gensym(), $class;
    $self->init(@_);
}

sub READLINE() {
    goto &getlines if wantarray;
    goto &getline;
}

sub BINMODE { 
    binmode shift()->handle;
}

sub GETC {
    die "Arbitrary GETCs not allowed";
}

sub PRINT {
    no warnings;
    shift()->print(@_);
}

sub PRINTF {
    no warnings;
    my $fh = shift()->handle;
    print $fh sprintf(@_);
}

sub READ {
    die "Arbitrary reads not allowed";
}

sub WRITE {
    die "Arbitrary writes not allowed";
}

sub SEEK {
    shift()->seek(@_);
}

sub TELL {
    shift()->tell;
}

sub EOF {
    shift()->eof;
}

sub CLOSE {
    shift()->close;
}

sub FILENO {
    no warnings;
    fileno shift()->handle;
}




# --- Functions

sub fh_close {
    my ($fh) = @_;
    if (UNIVERSAL::isa($fh, 'GLOB')) {
        no warnings;
        close $fh;
    } else {
        $fh->close;
    }
}

sub fh_seek {
    my ($fh, $pos, $whence) = @_;
    if (UNIVERSAL::isa($fh, 'GLOB')) {
        no warnings;
        seek $fh, $pos, $whence;
    } else {
        $fh->seek(@_);
    }
}

sub fh_tell {
    my ($fh) = @_;
    if (UNIVERSAL::isa($fh, 'GLOB')) {
        no warnings;
        tell $fh;
    } else {
        $fh->tell;
    }
}

sub fh_truncate {
    my ($fh, $length) = @_;
    if (UNIVERSAL::isa($fh, 'GLOB')) {
        no warnings;
        truncate $fh, $length;
    } else {
        $fh->truncate($length);
    }
}

sub fh_eof {
    my ($fh) = @_;
    if (UNIVERSAL::isa($fh, 'GLOB')) {
        no warnings;
        eof $fh;
    } else {
        $fh->eof;
    }
}


1;


=head1 NAME

IO::YAML - read and write YAML streams incrementally

=head1 SYNOPSIS

    use IO::YAML;
    
    $io = IO::YAML->new($path_or_filehandle);
    $io = IO::YAML->new(
        'path'      => '/path/to/a/file',
        'auto_load' => $bool,
    );
    $io = IO::YAML->new(
        'handle' => $fh,
        'mode'   => '>',  # or 'w'; '<' or 'r'; '>>' or 'a'
    );
    
    $io = IO::YAML->new;
    $io->open($path, '>')  or die $!;  # Open a stream for writing
    $io->open($path, '>>') or die $!;  # Open a stream for appending
    
    print $io $mystring;
    print $io \@myarray;
    print $io \%myhash;
    print $io $myobj;
    
    $io = IO::YAML->new;
    $io->open($path, '<')  or die $!;  # Open a stream for reading
    while (<$io>) {
        $data = YAML::Load($_);
    }
    
    $io = IO::YAML->new;
    $io->open($path) or die $!;  # Default mode is reading
    $io->auto_load(1);
    while (not $io->eof) {
        $data = <$io>;
    }
    
    $io = IO::YAML->new($path_or_handle);
    $io->auto_load(1);
    my @values = <$io>;  # Equivalent to YAML::LoadFile(...)

=head1 DESCRIPTION

B<IO::YAML> may be used to read and write YAML streams one C<document> (i.e.,
one value) at a time.

A YAML stream is a file consisting of a sequence of YAML documents; the stream
may optionally be followed by the end-of-stream marker (a line consisting solely
of the three-byte sequence "..."), after which any sequence of bytes may occur
(and will be ignored).

The first line of each document must begin with the three-byte sequence C<--->.

Here's a simple example consisting of three documents; their values are the
string 'foo', an empty array, and a hash with three elements:

    --- #YAML:1.0 foo
    --- #YAML:1.0 []
    --- #YAML:1.0
    title: Testing 1, 2, 3
    author: nkuitse
    date: 2004-03-05
    ...
    Blah blah blah ignored ignored ignored.
    ^D

(Here, C<^D> indicates the end of the file.)

In this next example, the stream consists of a single YAML document whose value
is C<undef>:

    --- ~
    ^D

As this example shows, the first line in each document need not contain the
full YAML 1.0 header; nor must the stream contain the end-of-stream marker.

=head2 Reading from a YAML stream

To read from a YAML stream, you may use the angle-brackets operator (e.g.,
E<lt>$fhE<gt>) or the equivalent methods C<getline> or C<read>.  Rather than
reading a single line, this will read an entire YAML document.

    while(defined(my $yaml = <$io>)) {
        my $value = YAML::Load($yaml);
        ...
    }

The C<YAML::Load> step may be omitted by setting the IO::YAML object's
C<auto_load> property to a true value:

    $io->auto_load(1);
    while(defined(my $value = <$io>)) {
        ...
    }

However, this example is complicated by the fact that the value of a YAML
document may be undef; the loop as written will terminate when the end of the
stream is reached I<or> when an undef value is read.

To avoid this problem while still taking advantage of the C<auto_load> property,
use C<< $io->eof >> to test for the end of the stream:

    $io->auto_load(1);
    while(not $io->eof) {
        my $value = <$io>;
        ...
    }

=head2 Writing to a YAML stream

To print to a YAML stream, call C<print> just as you would with a regular file
handle; the value(s) you're printing will be converted to YAML format before
being written:

    $io = IO::YAML->new;
    $io->open('>file') or die "Couldn't open 'file'";
    print $io $anything;

You can `print' anything that YAML is capable of serializing; an exception will
be raised if you attempt to print something that can't be serialized (e.g., a
reference to a subroutine).

The complication with undef values that affects the reading of a YAML stream
is not an issue when writing to a YAML stream.

=head1 METHODS

=over 4

=item B<new>

    $io = IO::YAML->new;
    
    # Concise forms
    $io = IO::YAML->new("$file");     # Default is read-only
    $io = IO::YAML->new("<$file");    # Read-only made explicit
    $io = IO::YAML->new(">$file");    # Read-write (empty header & body)
    $io = IO::YAML->new($file, '<');  # Or '>', '+<', 'r', etc.
    $io = IO::YAML->new(\*STDIN);
    $io = IO::YAML->new(\*STDOUT, '>');
    $io = IO::YAML->new($anything_that_isa_GLOB);
    
    # Full-fledged forms
    $io = IO::YAML->new(
        'path' => $file,        # File will be opened read-only
        'auto_load' => 1,       # Default is 0
    );
    $io = IO::YAML->new(
        'path' => $file,        # File will be opened or created
        'mode' => '>',          # Default is '<'; '>>' is also allowed
    );
    
Instantiate an IO::YAML object.  An exception is thrown if anything goes
wrong.

If a path is specified, the file at that path will be opened.  Otherwise,
you'll have to open it yourself using the C<open()> method.

If a path has been specified and the file doesn't already exist, it will be
created -- but only if you've specified a mode that permits writing; if you
haven't, an exception will be thrown.

The following arguments may be specified in the constructor:

=over 4

=item I<path>

Path to a file to create (if it doesn't already exist) and open.

=item I<mode>

Read/write/append mode for the new file.  This must be specified in one
of the following forms:

=over 4

=item E<lt>

=item E<gt>

=item E<gt>E<gt>

=item r

=item w

=item a

Modes that allow for both reading and writing are not allowed, since YAML
documents are variable in size.

=back

B<NOTE:> Numeric modes are not yet implemented.

=back

=item B<open>

    $io = IO::YAML->new;
    $io->open("<$file") or die $!;
    $io->open($file, $mode) or die $!;

Open a file with the specified name and mode.  You must use this method
if the instance was created without a C<path> element (and one has not
been assigned using the C<path()> method).

Upon failure, sets C<$!> to a meaningful message and returns a false
value.

The possible modes are as described for B<new>.

The C<open()> method may be called repeatedly on the same instance,
without having to close it.

=item B<close>

    $io->close or die $!;

Close the filehandle.

=item B<print>

=item B<getline>

=item B<getlines>

=item B<seek>

=item B<tell>

=item B<truncate>

=item B<seek>

=item B<seek>

=item B<seek>

=item B<seek>

=back

=head1 BUGS

Autoflush might not be working.

=head1 TO DO

Implement numeric modes.

Figure out how to allow read-write access, plus seek(), tell(), and truncate().

=head1 SEE ALSO

L<YAML|YAML>

=head1 AUTHOR

Paul Hoffman (nkuitse AT cpan DOT org)

=head1 COPYRIGHT

Copyright 2004 Paul M. Hoffman.

This is free software, and is made available under the same terms as
Perl itself.

