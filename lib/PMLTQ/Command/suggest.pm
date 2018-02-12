package PMLTQ::Command::suggest;

# ABSTRACT: Return query for given nodes

use PMLTQ::Base 'PMLTQ::Command';
use File::Path qw( make_path );

has usage => sub { shift->extract_usage };

sub run {
  my $self = shift;

  print STDERR "TODO: PMLTQ::Command::suggest\n";

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
