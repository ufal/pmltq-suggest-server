package PMLTQ::Command::suggest;

# ABSTRACT: Return query for given nodes

use PMLTQ::Base 'PMLTQ::Command';
use PMLTQ;
use PMLTQ::Suggest::Utils;
use PMLTQ::Suggest;
use File::Path qw( make_path );
use Getopt::Long qw(GetOptionsFromArray);
use Treex::PML;

has usage => sub { shift->extract_usage };

my %opts;

sub run {
  my $self = shift;
  my @args = @_;
  GetOptionsFromArray(\@args, \%opts,
    'nodes|N=s'
  )  || die "invalid options";
  Treex::PML::AddResourcePath(
       PMLTQ->resources_dir,
       File::Spec->catfile(${FindBin::RealBin},'config'),
       $ENV{HOME}.'/.tred.d',
       $self->config->{resources}
      );
  my @paths = $opts{nodes} ? split(/\|/, $opts{nodes}) : ();
  my @positions;
  foreach my $p (@paths) {
    my ($path, $goto)=PMLTQ::Suggest::Utils::parse_file_suffix($p);
    $path = URI->new($path)->canonical->as_string;
    push @positions, [$path,$goto];
  }
  my $pmltq = PMLTQ::Suggest::make_pmltq(
    \@positions
   );
  print "$pmltq\n";
}

=head1 SYNOPSIS

  pmltq suggest <treebank_config>

=head1 DESCRIPTION

Return query for given nodes. It works on local PML files.

=head1 OPTIONS

=head1 PARAMS

=over 5

=item B<treebank_config>

Path to configuration file. If a treebank_config is --, config is readed from STDIN.

=back

=cut

1;
