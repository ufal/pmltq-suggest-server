use strict;
use warnings;
use Test::Most;
use Socket;
use File::Find::Rule;
use File::Spec;
use File::Basename qw/dirname basename/;
use lib File::Spec->rel2abs( File::Spec->catdir( dirname(__FILE__),File::Spec->updir, 'lib' ) );
use Treex::PML;
use PMLTQ::Suggest::Server;

my $PORT = 40000 + int(rand(10000));

my $server = PMLTQ::Suggest::Server->new($PORT);

my @resources = File::Find::Rule->directory->name('resources')->in( File::Spec->catdir( dirname(__FILE__), 'treebanks') );
print STDERR "Adding resources\n\t",join("\n\t",@resources),"\n";
Treex::PML::SetResourcePaths(@resources);

my $pid=$server->background;
like($pid, '/^-?\d+$/', "PML-TQ suggest server is running. pid=$pid");


select(undef,undef,undef,0.2); # wait a sec
my @message_tests = (
      [["GET / HTTP/1.1",""], '/Not found/', '[GET] Not found'],
      [["POST / HTTP/1.1","Content-Length: 0",""], '/Not found/', '[POST] Not found'],
      [["HEAD / HTTP/1.1",""], '/Not found/', '[HEAD] Not found'],
      [["PUT / HTTP/1.1","Content-Length: 0",""], '/Not found/', '[PUT] Not found'],
      [["DELETE / HTTP/1.1",""], '/Not found/', '[DELETE] Not found'],
      [["PATCH / HTTP/1.1","Content-Length: 0",""], '/Not found/', '[PATCH] Not found'],
      [["OPTIONS / HTTP/1.1","Content-Length: 0",""], '/Not found/', '[OPTIONS] Not found'],

      [["GET /?p=treebanks/pdt_test/cmpr9410_001.t.gz HTTP/1.1",""], '/Not found/', '[GET] Not found - path has been permitted'],
      [["GET /?p=".File::Spec->rel2abs( dirname(__FILE__))."/treebanks/pdt_test/data/cmpr9410_001.t.gz HTTP/1.1",""], '/Not found/', '[GET] Not found - path does not contain address'],
      [["GET /?p=".File::Spec->rel2abs( dirname(__FILE__))."/FILE#ID HTTP/1.1",""], '/Internal server error/', '[GET] nonexisting file'],
      [["GET /?p=".File::Spec->rel2abs( dirname(__FILE__))."/treebanks/pdt_test/data/cmpr9410_001.t.gz#t-cmpr9410-001-p2s1w2 HTTP/1.1",""], '/t-cmpr9410-001-p2s1w2/', '[GET] #######'],




);
foreach my $message_test (@message_tests) {
  my ($message, $expected, $description) = @$message_test;
  like(fetch(@$message), $expected, $description);
  select(undef,undef,undef,0.2); # wait a sec
}


is(kill(9,$pid),1,'PML-TQ suggest server has been stopped.');
wait or die "counldn't wait for PML-TQ suggest server process completion";

done_testing();




sub fetch {
    my $hostname = "localhost";
    my $port = $PORT;
    my $message = join "", map { "$_\015\012" } @_;
    my $timeout = 5;    
    my $response;        
    
    eval {
        local $SIG{ALRM} = sub { die "early exit - SIGALRM caught" };
        alarm $timeout*2; #twice longer than timeout used later by select()  
 
        my $iaddr = inet_aton($hostname) || die "inet_aton: $!";
        my $paddr = sockaddr_in($port, $iaddr) || die "sockaddr_in: $!";
        my $proto = getprotobyname('tcp') || die "getprotobyname: $!";
        socket(SOCK, PF_INET, SOCK_STREAM, $proto) || die "socket: $!";
        connect(SOCK, $paddr) || die "connect: $!";
        (send SOCK, $message, 0) || die "send: $!";
        
        my $rvec = '';
        vec($rvec, fileno(SOCK), 1) = 1;
        die "vec(): $!" unless $rvec; 

        $response = '';
        for (;;) {        
            my $r = select($rvec, undef, undef, $timeout);
            die "select: timeout - no data to read from server" unless ($r > 0);
            my $l = sysread(SOCK, $response, 1024, length($response));
            die "sysread: $!" unless defined($l);
            last if ($l == 0);
        }
        $response =~ s/\015\012/\n/g; 
        (close SOCK) || die "close(): $!";
        alarm 0;
    }; 
    if ($@) {
      	return "[ERROR] $@";
    }
    else {
        return $response;
    }    
}