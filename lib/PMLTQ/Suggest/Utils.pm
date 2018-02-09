package PMLTQ::Suggest::Utils;

use Encode ();
use Treex::PML::Schema::CDATA;
use Treex::PML::Factory;
use UNIVERSAL;

#######################################################################################
# Usage         : first(\&sub, @list)
# Purpose       : Return the first element of list for which the sub returns true
#                 (no arguments are passed to the sub, it has to use $_);
#                 Return undef otherwise (or empty list in list context)
# Returns       : see Purpose
# Parameters    : anonymous_sub \&sub -- subroutine that does not take any arguments and
#                                         returns values which can be evaluated to true or false
#                 list @list -- first element from the @list, which is accepted by \&sub is then returned
# Throws        : no exceptions
# Comments      : Prototyped function
sub first (&@) {
    my $code = shift;

    foreach (@_) {
        return $_ if &{$code}();
    }

    return;
}
#######################################################################################
### from TrEd::Utils
# Usage         : apply_file_suffix($win, $goto)
# Purpose       : Set current tree and node positions to positions described by
#                 $goto suffix in file displayed in $win window
# Returns       : 1 if the new position was found and set, 0 otherwise
# Parameters    : TrEd::Window $win -- reference to TrEd::Window object
#                 string $goto      -- suffix of the file (or a position in the file)
# Throws        : no exceptions
# Comments      : Possible suffix formats:
#                   ##123.2 -- tree number 123 (if counting from 1) and its second node
#                   #123.3 -- tree whose $root->{form} equals to #123 and its third node
#                           (only hint found in Treex/PML/Backend/CSTS/Csts2fs.pm)
#                   #a123 -- finds node with id #a123 and the tree it belongs to
#                 The node's id can also be placed after the '.', e.g. ##123.#a123, in
#                 which case the sub searches for node with id #a123 inside tree no 123
#
#                 Sets $win->{treeNo} and $win->{currentNode} if appropriate.
# See Also      : parse_file_suffix()
sub apply_file_suffix {
    my ( $win, $goto ) = @_;
warn("apply file suffix");
    return if ( !defined $win );
    my $fsfile = $win->{FSFile};
    return if !( defined $fsfile && defined $goto && $goto ne ''); # $EMPTY_STR );

    if ( $goto =~ m/^##([0-9]+)/ ) {
warn("[1] apply file suffix");

        # handle cases like '##123'
        my $no = int( $1 - 1 );
        $win->{treeNo} = min( max( 0, $no ), $fsfile->lastTreeNo() );
        return 0 if $win->{treeNo} != $no;
    }
    elsif ( $goto =~ /^#([0-9]+)/ ) {
warn("[2] apply file suffix");

        # handle cases like '#123'
        # this is PDT 1.0-specific code, sorry
        my $no;
        for ( my $i = 0; $i <= $fsfile->lastTreeNo(); $i++ ) {
            if ( $fsfile->treeList()->[$i]->{form} eq "#$1" ) {
                $no = $i;
                last;
            }
        }
        return 0 if ( !defined $no );
        $win->{treeNo} = $no;
    }
    elsif ( $goto =~ /^#([^#]+)$/ ) {
warn("[3] apply file suffix");

        # handle cases like '#a123'
        my $id = $1;
        if ( Treex::PML::Schema::CDATA->check_string_format( $id, 'ID' ) ) {
            my $id_hash = $fsfile->appData('id-hash');
            if ( UNIVERSAL::isa( $id_hash, 'HASH' )
                && exists $id_hash->{$id} )
            {
warn("[3.1] apply file suffix");
                my $node = $id_hash->{$id};

                # we would like to use Treex::PML::Index() here, but can't
                # and why we can not?
                my $list = $fsfile->treeList();
                my $root = UNIVERSAL::can( $node, 'root' ) && $node->root();
                my $n    = defined($root) && first {
                    $list->[$_] == $root;
                }
                0 .. $#$list;
warn("[3.1_] apply file suffix");

                if ( defined $root and !defined($n) ) {
warn("[3.2] apply file suffix");
                    $n = _find_tree_no( $fsfile, $root, $list );

                    # exit from _find_tree_no() function
                    if ( !defined $n || $n == -1 ) {
                        return 0;
                    }
                }
                if ( defined($n) ) {
warn("[3.3] apply file suffix");
                    $win->{treeNo}      = $n;
                    $win->{currentNode} = $node;
                    return 1;
                }
                else {
warn("[3.4] apply file suffix");
                    return 0;
                }
            }
        }
    }

    # new: we're the dot in .[0-9]+ (TM)
    if ( $goto =~ /\.([0-9]+)$/ ) {
        my $root = get_node_by_no( $win, $1 );
        if ($root) {
            $win->{currentNode} = $root;
            return 1;
        }
        else {
            return 0;
        }
    }
    elsif ( $goto =~ /\.([^0-9#][^#]*)$/ ) {
        my $id = $1;
        if ( Treex::PML::Schema::CDATA->check_string_format( $id, 'ID' ) ) {
            my $id_hash = $fsfile->appData('id-hash');
            if ( UNIVERSAL::isa( $id_hash, 'HASH' )
                && exists( $id_hash->{$id} ) )
            {
                return 1
                    if ( $win->{currentNode} = $id_hash->{$id} ); # assignment
            }
            else {
                return 0;
            }
        }
    }
    return 1;

    # hey, caller, you should redraw after this!
}
#######################################################################################
### from TrEd::Utils
# Usage         : parse_file_suffix($filename)
# Purpose       : Split file name into file name itself and its suffix
# Returns       : List which contains file name and its suffix, if there is no suffix,
#                 second list element is undef
# Parameters    : scalar $filename -- name of the file
# Throws        : no exceptions
# Comments      : File suffix can be of the following forms:
#                 a) 1 or 2 #-signs, upper-case characters or numbers, and optionally followed by
#                     optional dash, full stop and at least one number
#                 b) 2 #-signs, at least one number, full stop, followed by
#                     one non-numeric not-# character and any number of not-# chars
#                 c) 1 #-sign followed by any number of not-# characters
# See Also      :
sub parse_file_suffix {
    my ($filename) = @_;
    #
    return if ( !defined $filename );
    if ( $filename =~ s/(##?[0-9A-Z]+(?:-?\.[0-9]+)?)$// ) {
        return ( $filename, $1 );
    }
    elsif (
        $filename =~ m{^
                        (.*)               # file name with any characters followed by
                        (\#\#[0-9]+\.)       # 2x#, at least one number and full stop
                        ([^0-9\#][^\#]*)     # followed by one non-numeric not-# character and any number of not-# chars
                        $
                        }x
        and Treex::PML::Schema::CDATA->check_string_format( $3, 'ID' )
        )
    {
        return ( $1, $2 . $3 );
    }
    elsif (
        $filename =~ m{^
                        (.*)        # file name with any characters followed by
                        \#          # one hash followed by
                        ([^\#]+)     # any number of not-# characters
                        $
                        }x
        and Treex::PML::Schema::CDATA->check_string_format( $2, 'ID' )
        )
    {
        return ( $1, '#' . $2 );
    }
    else {
        return ( $filename, undef );
    }
}

######################################



# open a data file and related files on lower layers
sub open_file {
  my $filename = shift;
  # TODO fsfile caching and closing !!!
  my $fsfile = Treex::PML::Factory->createDocumentFromFile($filename);
  if ($Treex::PML::FSError) {
    die "Error loading file $filename: $Treex::PML::FSError ($!)\n";
  }
  my $requires = $fsfile->metaData('fs-require');
  if ($requires) {
    for my $req (@$requires) {
      my $req_filename = $req->[1]->abs( $fsfile->URL );
      warn("REQUIRES $req_filename");
      my $secondary    = $fsfile->appData('ref');
      unless ($secondary) {
        $secondary = {};
        $fsfile->changeAppData( 'ref', $secondary );
      }
      my $sf = open_file($req_filename);
      $secondary->{ $req->[0] } = $sf;
    }
  }
  return $fsfile;
}
#############################################

1;