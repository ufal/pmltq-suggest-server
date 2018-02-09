package PMLTQ::Suggest;

use Encode ();
use Treex::PML::Schema::CDATA;
use Treex::PML::Factory;
use UNIVERSAL;
use TredMacro;




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





sub make_pmltq {
  my ($positions,%opts)=@_;


  if (exists &PMLTQextenssion::nodes_to_pmltq) {
warn("[0.0]\n");
##    my $cur = CurrentFile();
##    my @open_files = ($cur ? ($cur, TredMacro::GetSecondaryFiles($cur)) : ());
my @open_files;    
    my %cur_fsfiles; @cur_fsfiles{@open_files}=();
    # my $keep_cur;
    my %fsfiles;
    my @new_fsfiles;
warn("[0.1]\n");
    foreach my $f (map $_->[0], @$positions) {
warn("[1.0] LOOP $f\n");
      next if exists $fsfiles{$f};
#      my ($open) = grep { Treex::PML::IO::is_same_filename($_->filename, $f) }
#        @open_files;
#      if ($open) {
#        # $keep_cur=1 if !$keep_cur and exists($cur_fsfiles{$open});
#        $fsfiles{$f}=$open;
#      } else {
        my $fsfile = open_file($f);
warn("[1.1] fsfile: $fsfile\n");
        my @new = ($fsfile, TredMacro::GetSecondaryFiles($fsfile));
        push @new_fsfiles, @new;
        push @open_files, @new;
        $fsfiles{$_->filename}=$_ for @new; # including $fsfile
        $fsfiles{$f}=$fsfile; # $f may be different from $fsfile->filename
#      }      
    }
    my @nodes;
    for my $pos (@$positions) {
warn("LOOP pozitions @$pos");
      my $win = { FSFile => $fsfiles{$pos->[0]} };
      unless (apply_file_suffix($win,$pos->[1]) and $win->{currentNode}) {
        warn "Didn't find node [@$pos, @{[%$win]}]\n";
        return;
      }
      push @nodes, [ $win->{currentNode}, $win->{FSFile} ];
    }
    print STDERR "generating query";
    return PMLTQextenssion::nodes_to_pmltq(\@nodes,\%opts);
  }
  print STDERR "Method nodes_to_pmltq not available\n";
  return;
}


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
















package PMLTQextenssion;

use PMLTQ::Common qw(:all);
use Treex::PML::Schema::Constants;

sub nodes_to_pmltq {
  my ($nodes,$opts)=@_;
warn("PMLTQextenssion::nodes_to_pmltq");
  $opts||={};
  my %id_member;
  my $name = 'a';
  $name++ while $opts->{reserved_names}  && exists($opts->{reserved_names}{$name});
  my %node2name;
  $opts->{id2name} = { map {
    my $n = $_->[0];
    my $t = $n->type;
    my $id_member = ( $id_member{$t}||=_id_member_name($t) );
    my $var = $node2name{$n} = $name++;
    $name++ while $opts->{reserved_names}  && exists($opts->{reserved_names}{$name});
    ($n->{$id_member} => $var)
  } @$nodes };

  # discover relations;
  my %marked;
  @marked{map $_->[0], @$nodes}=(); # undef by default, 1 if connected
  my %parents=();
  my %connect;
  for my $m (@$nodes) {
warn("NODE LOOP $m");
    my ($n,$fsfile)=@$m;
    my $parent = $n->parent;
    $parents{$parent}||=$n;
    if ($parent and exists($marked{$parent})) {
      push @{$connect{$n->parent}{child}}, $n;
      # print STDERR "$node2name{$n->parent} has child $node2name{$n}\n";
      $marked{$n}=1;
    } elsif ($parents{$parent}!=$n) {
      push @{$connect{$parents{$parent}}{sibling}}, $n;
      # print STDERR "$node2name{$parents{$parent}} has sibling $node2name{$n}\n";
      $marked{$n}=1;
    } else {
      $parent = $parent && $parent->parent;
      while ($parent) {
	if (exists $marked{$parent}) {
	  # print STDERR "$node2name{$parent} has descendant $node2name{$n}\n";
	  push @{$connect{$parent}{descendant}}, $n;
	  $marked{$n}=1;
	  last;
	}
	$parent = $parent->parent;
      }
    }
  }
warn("End of LOOP @$nodes");
  $opts->{connect}=\%connect;
  return join(";\n\n", map {
    node_to_pmltq($_->[0],$_->[1],$opts)}
		grep { !$marked{$_->[0]} } @$nodes);
}

sub node_to_pmltq {
  my ($node,$fsfile,$opts)=@_;
warn("node_to_pmltq $node");
  return unless $node;
  my $type = $node->type;
  return unless $type;
  my $out='';
  my $indent = $opts->{indent} || '';

  my $var = $opts->{id2name} && $opts->{id2name}{$node->{_id_member_name($node->type)}};
  $var = ' $'.$var.' := ' if $var;
warn("[4.0] nodes_to_pmltq");
  $out .= PMLTQ::Common::DeclToQueryType($type).$var." [\n";
  foreach my $attr ('#name',$type->get_normal_fields) {
warn("[4.1] nodes_to_pmltq");
    my $m = $type->get_member_by_name($attr);
    # next if $m and $m->get_role() eq '#ID';
    my $val = $node->{$attr};
    next unless defined $val;
    $m = $type->get_member_by_name($attr.'.rf') unless $m;
    if ($attr eq '#name') {
warn("[4.2.1] $out");
      $out .= $indent.qq{  name() = }._pmltq_string($val).qq{,\n};
      next;
    } elsif (!$m) {
warn("[4.2.1] $out");
      $out .= $indent." # $attr ???;" unless $opts->{no_comments};
      next;
    }
    my $name = $attr eq '#content' ? 'content()' : $attr;
    next if $opts->{exclude} and $opts->{exclude}{$name};
    $out.=member_to_pmltq($name,$val,$m,$indent.'  ',$fsfile,$opts);
  }
warn("[5.0] nodes_to_pmltq");
  if (defined $opts->{rbrothers}) {
    $out .= $indent.qq{  # rbrothers()=$opts->{rbrothers},\n} unless $opts->{no_comments};
  }
  if ($opts->{connect}) {
    my $rels = $opts->{connect}{$node};
    if ($rels) {
      foreach my $rel (sort keys %$rels) {
	foreach my $n (@{$rels->{$rel}}) {
	  $out.='  '.$indent.$rel.' '.node_to_pmltq($n,$fsfile,{
	    %$opts,
	    indent=>$indent.'  ',
	  }).",\n";
	}
      }
    }
  } elsif ($opts->{children} or $opts->{descendants}) {
    my $i = 0;
    my $son = $node->firstson;
    while ($son) {
      $out.='  '.$indent.'child '.node_to_pmltq($son,$fsfile,{
	%$opts,
	indent=>$indent.'  ',
	children => 0,
	rbrothers=>$i,
      }).",\n";
      $i++;
      $son=$son->rbrother;
    }
    $out .= $indent.qq{  # sons()=$i,\n} unless $opts->{no_comments};
  }
  $out.=$indent.']';
  return $out;

}

sub _id_member_name {
  my ($type)=@_;
warn("_id_member_name");
  return undef unless $type;
  if ($type->get_decl_type == PML_ELEMENT_DECL) {
    $type = $type->get_content_decl;
  }
  my ($omember) = $type->find_members_by_role('#ID');
  if ($omember) {
    return ($omember->get_name);
  }
  return undef; # we want this undef
}

sub _pmltq_string {
  my ($string)=@_;
  $string=~s/([\\'])/\\$1/g;
  $string=~s/(\n)/\\n/g;
  return qq{'$string'};
}

sub resolve_pmlref {
  my ($ref,$fsfile)=@_;
warn("resolve_pmlref");
  if ($ref=~m{^(.+?)\#(.+)$}) {
    my ($file_id,$id)=($1,$2);
    my $refs = $fsfile->appData('ref');
    my $reffile = $refs && $refs->{$file_id};
    if (UNIVERSAL::DOES::does($reffile,'Treex::PML::Document')) {
warn("resolve_pmlref DOCUMENT");
      return PML::GetNodeByID($id,$reffile);
    } elsif (UNIVERSAL::DOES::does($reffile,'Treex::PML::Instance')) {
warn("resolve_pmlref INSTANCE");
      return $reffile->lookup_id($id);
    }
  } elsif ($ref=~m{\#?([^#]+)}) {
warn("resolve_pmlref LOCAL $1");
    return PML::GetNodeByID($1);
  }
  return undef;
}

sub member_to_pmltq {
  my ($name, $val, $type, $indent, $fsfile, $opts)=@_;
  my $out;
warn("[6.0] member_to_pmltq");
  my $mtype = $name eq 'content()' ? $type : $type->get_knit_content_decl;
  if ($mtype->get_decl_type == PML_ALT_DECL and !UNIVERSAL::DOES::does($val,'Treex::PML::Alt')) {
    $mtype = $mtype->get_knit_content_decl;
  }
  if (not ref($val)) {
warn("[6.0.0] member_to_pmltq");
    if (!$mtype->is_atomic) {
      $out.=$indent."# ignoring $name\n",
    } else {
      my $is_pmlref = (($mtype->get_decl_type == PML_CDATA_DECL) and ($mtype->get_format eq 'PMLREF')) ? 1 : 0;
      if ($type and ($type->get_role() =~ /^#(ID|ORDER)$/ or $is_pmlref)) {
	if ($is_pmlref and $opts->{id2name} and $val=~/(?:^.*?\#)?(.+)$/ and $opts->{id2name}{$1}) {
	  $out .= $indent.qq{$name \$}.$opts->{id2name}{$1}.qq{,\n};
	} elsif ($is_pmlref) {
	  my $target = resolve_pmlref($val,$fsfile);
	  if ($target && $target->type) {
	    $out.=$indent.'# '.$name.' '.PMLTQ::Common::DeclToQueryType( $target->type ).qq{ [ ],\n};
	  } else {
	    $out.=$indent.'# '.$name.qq{->[ ],\n};
	  }
	} elsif ($opts->{no_comments}) {
	  return;
	} else {
	  $out.=$indent.'# '.qq{$name = }._pmltq_string($val).qq{,\n};
	}
      } else {
	$out.=$indent;
	$out.=qq{$name = }._pmltq_string($val).qq{,\n};
      }
    }
  } elsif (UNIVERSAL::DOES::does($val,'Treex::PML::List')) {
warn("[6.0.1] member_to_pmltq");
    if ($mtype->is_ordered) {
      my $i=1;
      foreach my $v (@$val) {
	$out.=member_to_pmltq("$name/[$i]",$v,$mtype,$indent,$fsfile,$opts);
	$i++;
      }
    } else {
      foreach my $v (@$val) {
	$out.=member_to_pmltq($name,$v,$mtype,$indent,$fsfile,$opts);
      }
    }
  } elsif (UNIVERSAL::DOES::does($val,'Treex::PML::Alt')) {
    foreach my $v (@$val) {
      $out.=member_to_pmltq($name,$v,$mtype,$indent,$fsfile,$opts);
    }
  } elsif (UNIVERSAL::DOES::does($val,'Treex::PML::Struct') or UNIVERSAL::DOES::does($val,'Treex::PML::Container')) {
    $out.=$indent.qq{member $name \[\n};
    foreach my $attr ($mtype->get_normal_fields) {
      my $m = $mtype->get_member_by_name($attr);
      # next if $m and $m->get_role() eq '#ID';
      my $v = $val->{$attr};
      next unless defined $v;
      $m = $mtype->get_member_by_name($attr.'.rf') unless $m;
      if (!$m) {
	$out .= " # $attr ???;" unless $opts->{no_comments};
	next;
      }
      my $n = $attr eq '#content' ? 'content()' : $attr;
      next if $opts->{exclude} and $opts->{exclude}{$n};
      $out.=member_to_pmltq($n,$v,$m,$indent.'  ',$fsfile,$opts);
    }
    $out.=$indent.qq{],\n}
  } elsif (UNIVERSAL::DOES::does($val,'Treex::PML::Seq')) {
    my $i=1;
    foreach my $v ($val->elements) {
      my $n = $v->name;
      next if $opts->{exclude} and $opts->{exclude}{$n};
      $out.=member_to_pmltq("$name/[$i]$n",$v->value,$mtype->get_element_by_name($n),$indent,$fsfile,$opts);
      $i++;
    }
  }
warn("[6.1] member_to_pmltq $out");
  return $out;
}

package PML;

use Scalar::Util qw(weaken);

=item PML::GetNodeByID($id_or_ref,$fsfile?)

Looks up a node from the current file (or given fsfile) by its ID (or
PMLREF - i.e. the ID preceded by a file prefix of the form C<xy#>).

=cut

sub GetNodeByID {
    my ( $rf, $fsfile ) = @_;
    if (!defined $fsfile) {
    	warn("GetNodeByID TODO: FIX THIS !!!");
        #$fsfile = $grp->{FSFile};
    }
    $rf =~ s/^.*#//;
    return GetNodeHash($fsfile)->{$rf};
}

=item PML::GetNodeHash($fsfile?)

Return a reference to a hash indexing nodes in a given file (or the
current file if no argument is given). If such a hash was not yet
created, it is built upon the first call to this function (or other
functions calling it, such as C<GetNodeByID>. Use C<clearNodeHash> to
clear the hash.

=cut

sub GetNodeHash {
    if (!ref $_[0]) {
        shift;
    }
    warn("GetNodeHash TODO: fix this:");
    #my $fsfile = $_[0] || $grp->{FSFile};
    my $fsfile = $_[0];
    return {} if !ref $fsfile;
    if ( !ref $fsfile->appData('id-hash') ) {
        my %ids;
        my $trees = $fsfile->treeList();
        for ( my $i = 0; $i <= $#{$trees}; $i++ ) {
            my $node = $trees->[$i];
            while ($node) {
                weaken( $ids{ $node->{id} } = $node );
            }
            continue {
                $node = $node->following;
            }
        }
        $fsfile->changeAppData( 'id-hash', \%ids );
    }
    return $fsfile->appData('id-hash');
}


1;
