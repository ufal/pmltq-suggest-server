package TredMacro;

use strict;
use warnings;

use Treex::PML::Document;
use List::MoreUtils 'uniq';
use File::Basename 'basename';
use UNIVERSAL::DOES;

sub GetSecondaryFiles {
  my ($fsfile) = @_;
  # is probably the same as Treex::PML::Document->relatedDocuments()
  # a reference to a list of pairs (id, URL)
  my $requires = $fsfile->metaData('fs-require');
  my @secondary;
  if ($requires) {
    foreach my $req (@$requires) {
      my $id = $req->[0];
      my $req_fs
        = ref( $fsfile->appData('ref') )
          ? $fsfile->appData('ref')->{$id}
          : undef;
      if ( UNIVERSAL::DOES::does( $req_fs, 'Treex::PML::Document' ) ) {
        push( @secondary, $req_fs );
      }
    }
  }
  return uniq(@secondary);
}


sub OpenSecondaryFiles { 
    my ( $fsfile ) = @_;
    my $win = undef;
    my $status = 1;
    return $status if $fsfile->appData('fs-require-loaded');
    $fsfile->changeAppData( 'fs-require-loaded', 1 );
    my $requires = $fsfile->metaData('fs-require'); #$fsfile->relatedDocuments()
    if (defined $requires) {
        for my $req (@$requires) {
            next if ref( $fsfile->appData('ref')->{ $req->[0] } );
            my $req_filename
                = Treex::PML::ResolvePath( $fsfile->filename, $req->[1] );
            print STDERR "Pre-loading dependent $req_filename ($req->[1]) as appData('ref')->{$req->[0]}\n";
            my ( $req_fs, $status2 ) = open_file( # TODO simplify Tred::File::open_file() subrutine
                $win, $req_filename,
                -preload  => 1,
                -norecent => 1
            );
            _merge_status( $status, $status2 );
            if ( !$status2->{ok} ) {
                close_file( $win, -fsfile => $req_fs, -no_update => 1 );
                return $status2;
            }
            else { #zaznac do zavisleho, ze je zavisly na nadradenom
                push @{ $req_fs->appData('fs-part-of') },
                    $fsfile;    # is this a good idea?
                main::__debug("Setting appData('ref')->{$req->[0]} to $req_fs");
                $fsfile->appData('ref')->{ $req->[0] } = $req_fs;
            }
        }
    }
    return $status;
}

sub ThisAddress {
  my ($node, $fsfile) = @_;
  my $type = $node->type;
  my ($id_attr) = $type && $type->find_members_by_role('#ID');

  return basename($fsfile->filename) . '#' . $node->{ $id_attr->get_name }
}

sub GetNodeIndex {
  my $node = shift;
  my $i = -1;
  while ($node) {
    $node = $node->previous();
    $i++;
  }
  return $i;
}

1;
