#!/usr/bin/perl
use warnings;
use strict;
use Getopt::Long;
use Pod::Usage;

our $REVISION = '$Revision: c28f203cd058926c8e07c64feb03dfdc0814120e $';
our $DATE =	'$Date: 2015-04-05 22:21:04 -0700 (Sun,  5 Apr 2015) $';  
our $AUTHOR =	'$Author: Kai Wang <kai@openbioinformatics.org> $';

our ($verbose, $help, $man);
our ($dbtype, $dbfile, $outfile, $vcffile, $dbsnpfile, $buildver, $transcriptid, $cdotfile, $afstring, $acstring, $infostring, $twopos, $dbnsfpver);

our %iupac = (R=>'AG', Y=>'CT', S=>'GC', W=>'AT', K=>'GT', M=>'AC', A=>'AA', C=>'CC', G=>'GG', T=>'TT', B=>'CGT', D=>'AGT', H=>'ACT', V=>'ACG', N=>'ACGT', '.'=>'-', '-'=>'-');

GetOptions('verbose'=>\$verbose, 'help|h'=>\$help, 'man'=>\$man, 'outfile=s'=>\$outfile, 'dbtype=s'=>\$dbtype, 'vcffile=s'=>\$vcffile,
	'dbsnpfile=s'=>\$dbsnpfile, 'buildver=s'=>\$buildver, 'transcriptid=s'=>\$transcriptid, 'cdotfile=s'=>\$cdotfile, 'afstring=s'=>\$afstring,
	'acstring=s'=>\$acstring, 'infostring=s'=>\$infostring, 'twopos'=>\$twopos, 'dbnsfpver=s'=>\$dbnsfpver) or pod2usage ();

$help and pod2usage (-verbose=>1, -exitval=>1, -output=>\*STDOUT);
$man and pod2usage (-verbose=>2, -exitval=>1, -output=>\*STDOUT);
@ARGV or pod2usage (-verbose=>0, -exitval=>1, -output=>\*STDOUT);
@ARGV == 1 or pod2usage ("Syntax error");

($dbfile) = @ARGV;

$dbtype or pod2usage ("Error in argument: please specify --dbtype argument");
$dbtype eq 'cosmic' and $vcffile || pod2usage ("Error in argument: please specify --vcffile argument");

if ($outfile) {
	open (STDOUT, ">$outfile") or die "Error: cannot write to output file $outfile: $!\n";
}



if ($dbtype eq 'clinvar') {
	prepareClinVar ($dbfile);
} elsif ($dbtype eq 'cosmic') {
	prepareCosmic ($dbfile, $vcffile);
} else {
	pod2usage ("Error: the -dbtype of $dbtype is not supported yet");
}





sub prepareClinVarOld {
	my ($dbfile) = @_;
	open (FH, "convert2annovar.pl -format vcf4 -include $dbfile |") or die "Error: cannot read from dbfile $dbfile: $!\n";
	
	if ($outfile) {
		open (STDOUT, ">$outfile") or die "Error: cannot write to output file: $!\n";
	}
	
	my %sig = (0 => 'unknown', 1 => 'untested', 2 => 'non-pathogenic', 3 => 'probable-non-pathogenic', 4 => 'probable-pathogenic', 5 => 'pathogenic', 6 => 'drug-response', 7 => 'histocompatibility', 255 => 'other');
	while (<FH>) {
		m/^#/ and next; 
		s/[\r\n]+$//;
		my @field  = split(/\t/, $_);
		
		$field[12]=~m/(CLNDBN=([^;]+);.*CLNACC=([^;]+))/ or die "Error: invalid record found in avinputfile: <$field[12]>";
		my $clndbnacc = $1;
		
		$field[12]=~m/(CLNDSDB=([^;]+);.*CLNDSDBID=([^;]+))/ or die "Error: invalid record found in avinputfile: <$field[12]>";
		my $clndsdb = $1;
		
		$field[12]=~m/CLNSIG=([\d\|\,]+)/ or die "Error: invalid record found in avinput file: <$field[12]>";	#CLNSIG=5|5|5,0|0; in RCV000013623.23 
		my @sigall = split (/\|/, $1);		#CLNSIG=5|2|2;
		my (@clnsig, $clnsig);
		for my $i (0 .. @sigall-1) {
			if ($sigall[$i] =~ m/,/) {
				my @temp = split (/,/, $sigall[$i]);
				@temp = map {$sig{$_} || 'unknown'} @temp;
				push @clnsig, join(",", @temp);
			} else {
				push @clnsig, $sig{$sigall[$i]} || 'unknown';
			}
		}
		$clnsig = join ('|', @clnsig);
		
		print join ("\t", @field[0..4]), "\t", "CLINSIG=$clnsig;$clndbnacc;$clndsdb\n";

	}
}

sub prepareClinVar {
	my ($dbfile) = @_;
	open (FH, "convert2annovar.pl -format vcf4 -include $dbfile |") or die "Error: cannot read from dbfile $dbfile: $!\n";
	
	if ($outfile) {
		open (STDOUT, ">$outfile") or die "Error: cannot write to output file: $!\n";
	}
	
	#my %sig = (0 => 'unknown', 1 => 'untested', 2 => 'non-pathogenic', 3 => 'probable-non-pathogenic', 4 => 'probable-pathogenic', 5 => 'pathogenic', 6 => 'drug-response', 7 => 'histocompatibility', 255 => 'other');
	my %sig = (0 => 'Uncertain significance', 1 => 'not provided', 2 => 'Benign', 3 => 'Likely benign', 4 => 'Likely pathogenic', 5 => 'Pathogenic', 6 => 'drug response', 7 => 'histocompatibility', 255 => 'other');
	while (<FH>) {
		m/^#/ and next; 
		s/[\r\n]+$//;
		my @field  = split(/\t/, $_);
		
		$field[12]=~m/(CLNDBN=([^;]+);.*CLNACC=([^;]+))/ or die "Error: invalid record found in avinputfile: <$field[12]>";
		#my $clndbnacc = $1;
		my ($clndbn, $clnacc) = ($2, $3);
		
		$field[12]=~m/(CLNDSDB=([^;]+);.*CLNDSDBID=([^;]+))/ or die "Error: invalid record found in avinputfile: <$field[12]>";
		#my $clndsdb = $1;
		my ($clndsdb, $clndsdbid) = ($2, $3);
		
		$field[12]=~m/CLNSIG=([\d\|\,]+)/ or die "Error: invalid record found in avinput file: <$field[12]>";	#CLNSIG=5|5|5,0|0; in RCV000013623.23 
		my @sigall = split (/\|/, $1);		#CLNSIG=5|2|2;
		my (@clnsig, $clnsig);
		for my $i (0 .. @sigall-1) {
			if ($sigall[$i] =~ m/,/) {
				my @temp = split (/,/, $sigall[$i]);
				@temp = map {$sig{$_} || 'unknown'} @temp;
				push @clnsig, join(",", @temp);
			} else {
				push @clnsig, $sig{$sigall[$i]} || 'unknown';
			}
		}
		$clnsig = join ('|', @clnsig);
		
		$clnsig =~ s/,/\\x2c/g;
		$clndbn =~ s/,/\\x2c/g;
		$clndsdb =~ s/,/\\x2c/g;
		$clndsdbid =~ s/,/\\x2c/g;
		
		print join ("\t", @field[0..4], $clnsig, $clndbn, $clnacc, $clndsdb, $clndsdbid), "\n";

	}
}

sub prepareCosmic {
	my ($dbfile, $vcffile) = @_;
	my %mut;		#key=mutation ID value=chr+pos+ref+alt
	my (%cosmic, %cosmicid, %nonfound);
	my ($idprefix);
	open (VCF, $vcffile) or die "Error: cannot read from VCF file: $!\n";
	$_ = <VCF>;
	m/^##fileformat=VCFv4/ or die "Error: the supplied VCF file does not have valid version 4 header\n";
	while (<VCF>) {
		m/^#/ and next;
		my @field = split (/\t/, $_);
		
		#following paragraph is no longer necessary with new version of COSMIC
		#if ($field[2] =~ s/^COSM//) {		#for coding variants
		#	$idprefix = "COSM";
		#} elsif ($field[2] =~ s/^COSN//) {		#for non-coding variants
		#	$idprefix = "COSN";
		#} else {
		#	die "Error: invalid record found in VCF file: ID does not start with COSM or COSN: <$_>\n";
		#}
		
		
		my ($chr, $start, $id, $ref_allele, $mut_allele) = @field[0..4];
		my $end = $start + length($ref_allele) - 1;
		
		$ref_allele =~ m/[^ACGTacgt]/ and next;
		$mut_allele =~ m/[^ACGTacgt]/ and next;
		
		if (length ($ref_allele) > 1 or length ($mut_allele) > 1) {
			if(length($ref_allele) > length ($mut_allele)) { 		# deletion or block substitution
				my $head = substr($ref_allele, 0, length ($mut_allele));
				if ($head eq $mut_allele) {
					$start = $start+length($head);			#then change start position
					
					$ref_allele = substr ($ref_allele, length ($mut_allele));

					$mut_allele = "-";
				} elsif (substr ($ref_allele, 0, 1) eq substr ($mut_allele, 0, 1)) {	#first base is identical (this is used in many VCF files)
					$start++;
					$ref_allele = substr ($ref_allele, 1);
					$mut_allele = substr ($mut_allele, 1);
				}
					
			} elsif(length($mut_allele) >= length ($ref_allele)) { 		# insertion or block substitution
				my $head = substr ($mut_allele, 0, length ($ref_allele));
				if ($head eq $ref_allele) {
					$start = $start+length($ref_allele)-1;
					
					$mut_allele = substr ($mut_allele, length ($ref_allele));
					$ref_allele = '-';
				} elsif (substr ($ref_allele, 0, 1) eq substr ($mut_allele, 0, 1)) {	#first base is identical (this is used in many VCF files)
					$start++;
					$ref_allele = substr ($ref_allele, 1);
					$mut_allele = substr ($mut_allele, 1);
				}
			}
		}
		

		$mut{$id} = join ("\t", $chr, $start, $end, $ref_allele, $mut_allele);
	}
	print STDERR "NOTICE: Finished reading ", scalar (keys %mut), " mutation ID from the VCF file $vcffile\n";
	
	open (DB, $dbfile) or die "Error: cannot read from DB file: $!\n";
	$_ = <DB>;
	my @field = split (/\t/, $_);
	$field[16] eq 'Mutation ID' or die "Error: COSMIC MutantExport format error: column 17 should be 'Mutation ID'\n";
	while (<DB>) {
		@field = split (/\t/, $_);
		my $chrstring = $mut{$field[16]};
		if (not defined $chrstring) {
			$nonfound{$field[16]}++;
			next;
		}
		$cosmic{$chrstring} .= ";$field[6],$field[7]";		#these two columns are "ID_tumour" and "Primary site"
		#$cosmicid{$chrstring} .= ",$idprefix$field[16]";	#this is no longer necessary with new version of COSMIC
		$cosmicid{$chrstring} .= ",$field[16]";
	}
	print STDERR "NOTICE: Finished reading ", scalar (keys %cosmic), " COSMIC records in DB file $dbfile\n";
	print STDERR "WARNING: ", scalar (keys %nonfound), " COSMIC ID from MutantExport file cannot be found in VCF file (this may be normal if the VCF file only contains coding or noncoding variants\n";
	
	#the following is to eliminate duplicate entries. For example, COSM256593 and COSM256594 refers to the same mutation in the same sample, but are annotated twice, once with CDK11B/NM_001787 and once with CDC2L2/ENST00000357760
	
	for my $key (keys %cosmic) {
		my @id_site = split (/;/, $cosmic{$key});
		shift @id_site;
		my (%found_tumorid, %found_site);
		for my $i (0 .. @id_site-1) {
			my ($tumorid, $site) = split (/,/, $id_site[$i]);
			$found_tumorid{$tumorid} and next;		#the idea is that for duplicate entries, the tumor ID must be identical
			$found_tumorid{$tumorid}++;
			$found_site{$site}++;
		}
		
		my @cosmicid;
		@cosmicid = split (/,/, $cosmicid{$key});
		shift @cosmicid;
		my %cosmicid = map {$_, 1} @cosmicid;
		
		print $key, "\t", "ID=", join (",", keys %cosmicid), ";", "OCCURENCE=";
		my $occurence;
		for my $site (keys %found_site) {
			$occurence .= "," . $found_site{$site} . "(" . $site . ")";
		}
		$occurence =~ s/^,//;
		print "$occurence\n";
	}			
}


sub sum {
	my $sum = 0;
	for (@_) {
		$sum += $_;
	}
	return $sum;
}


sub revcom {
	my ($seq) = @_;
	$seq = reverse $seq;
	$seq =~ tr/acgtACGT/tgcaTGCA/;
	return ($seq);
}


=head1 SYNOPSIS

 prepare_annovar_user.pl [arguments] <dbfile | stdin>

 Optional arguments:
        -h, --help                      print help message
        -m, --man                       print complete documentation
        -v, --verbose                   use verbose output
            --outfile <file>		output file name
            --dbtype <string>		specify database type
            --afstring <string>		string denoting AF record in 1000gsite
            --acstring <string>		string denoting AC record in 1000gsite
            --infostring <string>	string denoting necessary information in 1000gsite
            --twopos			output two position column (for -dbtype 1000gsite)
            --dbnsfpver <string>	version of dbNSFP

 Function: reformat and prepare ANNOVAR annotation database
        
 Example: prepare_annovar_user.pl -dbtype clinvar clinvar.vcf > hg19_clinvar.txt
          prepare_annovar_user.pl -dbtype cosmic CosmicMutantExport.tsv  -vcf CosmicCodingMuts.vcf > hg38_cosmic76.txt
          prepare_annovar_user.pl -dbtype cosmic CosmicMutantExport.tsv  -vcf CosmicNonCodingVariants.vcf >> hg38_cosmic76.txt
 
 Version: $Date: 2015-04-05 22:21:04 -0700 (Sun,  5 Apr 2015) $

=head1 OPTIONS

=over 8

=item B<--help>

print a brief usage message and detailed explanation of options.

=item B<--man>

print the complete manual of the program.

=item B<--verbose>

use verbose output.

=item B<--outfile>

specify the output file name. By default, output is written to STDOUT.

=item B<--dbtype>

specify database type, such as esp, cosmicgff, hgmdgff, etc

=back

=head1 DESCRIPTION

This program is used to convert various databases to a format that can be used 
by ANNOVAR.

For questions or comments, please contact kai@openbioinformatics.org.

=cut