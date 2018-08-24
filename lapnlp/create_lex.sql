-- Table definitions for data in the Specialist Lexicon tables.
-- I am unsure what indexes should really be defined.
-- Created June 2009, Peter Szolovits, MIT
-- Free software.

drop table if exists lragr;

create table lragr (
       eui char(8) not null,
       str varchar(255) not null,
       sca char(5) not null,
       agr varchar(255),
       cit varchar(255),
       bas varchar(255),
       index lragr_eui (eui),
       index lragr_str (str),
       index lragr_sca (sca));

load data local infile "LRAGR" into table lragr
     fields terminated by '|';

drop table if exists lrtyp;

create table lrtyp (
       eui char(8) not null,
       bas varchar(255) not null,
       sca char(5) not null,
       typ varchar(255) not null,
       index lrtyp_eui (eui),
       index lrtyp_bas (bas),
       index lrtyp_sca (sca));

load data local infile "LRTYP" into table lrtyp
     fields terminated by '|';

drop table if exists lrcmp;

create table lrcmp (
       eui char(8) not null,
       bas varchar(255) not null,
       sca char(5) not null,
       com varchar(255),
       index lrcmp_eui (eui),
       index lrcmp_bas (bas),
       index lrcmp_sca (sca));

load data local infile "LRCMP" into table lrcmp
     fields terminated by '|';

drop table if exists lrprn;

create table lrprn (
       eui char(8) not null,
       bas varchar(255) not null,
       agr varchar(255),
       gnd varchar(255),
       cas varchar(255),
       pos varchar(255),
       qnt varchar(255),
       fea varchar(255));

load data local infile "LRPRN" into table lrprn
     fields terminated by '|';

drop table if exists lrmod;

create table lrmod (
       eui char(8) not null,
       bas varchar(255) not null,
       sca char(5) not null,
       psn_mod varchar(255),
       fea varchar(255));

load data local infile "LRMOD" into table lrmod
     fields terminated by '|';

drop table if exists lrprp;

create table lrprp (
       eui char(8) not null,
       bas varchar(255) not null,
       str varchar(255),
       sca varchar(255) not null,
       fea varchar(255));

load data local infile "LRPRP" into table lrprp
     fields terminated by '|';

drop table if exists lrabr;

create table lrabr (
       eui char(8) not null,
       bas varchar(255) not null,
       agr varchar(255) not null,
       euix char(8) not null,
       str varchar(255));

load data local infile "LRABR" into table lrabr
     fields terminated by '|';

drop table if exists lrspl;

create table lrspl (
       eui char(8) not null,
       spv varchar(255) not null,
       bas varchar(255) not null,
       index lrspl_eui (eui),
       index lrspl_spv (spv),
       index lrspl_bas (bas));

load data local infile "LRSPL" into table lrspl
     fields terminated by '|';

drop table if exists lrnom;

create table lrnom (
       eui char(8) not null,
       bas varchar(255) not null,
       sca char(5) not null,
       eui2 char(8) not null,
       bas2 varchar(255) not null,
       sca2 char(5) not null);

load data local infile "LRNOM" into table lrnom
     fields terminated by '|';

drop table if exists lrtrm;

create table lrtrm (
       eui char(8) not null,
       bas varchar(255) not null,
       gen varchar(255));

load data local infile "LRTRM" into table lrtrm
     fields terminated by '|';

drop table if exists lrwd;

create table lrwd (
       wrd varchar(255) not null,
       eui char(8) not null);

load data local infile "LRWD" into table lrwd
     fields terminated by '|';

drop table if exists lrfil;

create table lrfil (
       fil varchar(255),
       des varchar(255),
       fmt varchar(255),
       cls integer,
       rws integer,
       bts integer);

load data local infile "LRFIL" into table lrfil
     fields terminated by '|';

drop table if exists lrfld;

create table lrfld (
       col char(3) not null,
       des varchar(255) not null,
       ref varchar(255) not null,
       fil varchar(255) not null);

load data local infile "LRFLD" into table lrfld
     fields terminated by '|';

