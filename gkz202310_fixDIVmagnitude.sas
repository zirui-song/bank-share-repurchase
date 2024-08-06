%let wrds = wrds.wharton.upenn.edu 4016; 
options comamid=TCP remote=WRDS;
signon username=_prompt_;
Libname rwork slibref=work server=wrds;

libname mydir "C:\Users\nmg75\Dropbox\Research\017 - GKV - Repurchases\Data";


*Winsorization/Truncation macro;

%macro WT(data=_last_, out=, byvar=none, vars=, type = W, pctl = 1 99, drop= N);
 
      %if &out = %then %let out = &data;
   
      %let varLow=;
      %let varHigh=;
      %let xn=1;
 
      %do %until (%scan(&vars,&xn)= );
      %let token = %scan(&vars,&xn);
      %let varLow = &varLow &token.Low;
      %let varHigh = &varHigh &token.High;
      %let xn = %EVAL(&xn + 1);
      %end;
 
      %let xn = %eval(&xn-1);
 
      data xtemp;
            set &data;
 
      %let dropvar = ;
      %if &byvar = none %then %do;
            data xtemp;
            set xtemp;
            xbyvar = 1;
 
      %let byvar = xbyvar;
      %let dropvar = xbyvar;
      %end;
 
      proc sort data = xtemp;
            by &byvar;
 
      /*compute percentage cutoff values*/
      proc univariate data = xtemp noprint;
      by &byvar;
      var &vars;
      output out = xtemp_pctl PCTLPTS = &pctl PCTLPRE = &vars PCTLNAME = Low High;
 
      data &out;
      merge xtemp xtemp_pctl; /*merge percentage cutoff values into main dataset*/
      by &byvar;
      array trimvars{&xn} &vars;
      array trimvarl{&xn} &varLow;
      array trimvarh{&xn} &varHigh;
 
      do xi = 1 to dim(trimvars);
                  /*winsorize variables*/
            %if &type = W %then %do;
                  if trimvars{xi} ne . then do;
                        if (trimvars{xi} < trimvarl{xi}) then trimvars{xi} = trimvarl{xi};
                        if (trimvars{xi} > trimvarh{xi}) then trimvars{xi} = trimvarh{xi};
                  end;
            %end;
                  /*truncate variables*/
            %else %do;
                  if trimvars{xi} ne . then do;
                        if (trimvars{xi} < trimvarl{xi}) then trimvars{xi} = .T;
                        if (trimvars{xi} > trimvarh{xi}) then trimvars{xi} = .T;
                  end;
            %end;
 
                  %if &drop = Y %then %do;
                     if trimvars{xi} = .T then delete;
                  %end;
 
            end;
      drop &varLow &varHigh &dropvar xi;
 
      /*delete temporary datasets created during macro execution*/
      proc datasets library=work nolist;
            delete xtemp xtemp_pctl; quit; run;
 
%mend;


%macro dummy( 
   data=_last_ ,    /* name of input dataset                  */
   out=&data,       /* name of output dataset                 */
   var= ,           /* variable(s) to be dummied              */
   base=_last_,     /* base category                          */
   prefix = D_,     /* prefix for dummy variable names        */
   format =,        /* format used to categorize variable     */
   name  = VAL,     /* VAL: variable names are D_value        */
   fullrank=1       /* Eliminate dummy for baseline category? */
   );

	%let abort = 0;
   %if (%length(&var) = 0) %then %do;
       %put ERROR: DUMMY: VAR= must be specified;
		 %let abort=1;
       %goto done;
       %end;

%let base = %upcase(&base);
%let name = %upcase(&name);

%if %upcase(&data) = _LAST_ %then %let data = &syslast;
%if %upcase(&data) = _NULL_ %then %do;
	%put ERROR: There is no default input data set (_LAST_ is _NULL_);
	%let abort=1;
	%goto DONE;
	%end;
	
options nonotes;

%*-- Initialize output data set;
%if &out ^= &data %then %do;
	data &out;
		set &data;
	%end;
	
%let prefix = %upcase(&prefix);

%*-- j indexes variables, vari is the current variable name;
%local j vari;
%let j=1;
%*-- Find the current variable name;
%let vari= %scan(&var,    &j, %str( ));

%******************************************************************;
%*-- Loop over variables; 
%******************************************************************;
%do %while(&vari ^= );

	%*-- Find the current prefix for dummies;
	%let pre = %scan(&prefix, &j, %str( ));
	%if &pre = VARNAME | &pre = %then %let pre=&vari._;
	%*-- Keyword BLANK for prefix indicates no prefix;
	%if &pre=BLANK %then %let pre=;

	%*-- Find the current base for dummies;
	%let baseval = %scan(&base, &j, %str( ));
	%if &baseval = %then %let baseval=_LAST_;

	%*-- Find the current format for dummies;
	%let fmt = %scan(&format, &j, %str( ));

*-- determine values of variable to be dummied;
proc summary data = &out nway ;
     class &vari ;
     %if %length(&fmt) gt 0 %then %do;
	  		%*-- Make sure format name includes a '.';
        %if "%substr(&fmt, %length(&fmt))" ne "." 
		  		%then %let fmt = &fmt..;
        format &vari &fmt;
     %end;
     output out = _cats_ ( keep = &vari ) ;
	%if &syserr > 4 %then %let abort=1; 
	%if &abort %then %goto DONE;

	%if &fullrank %then %do;
	*-- Eliminate the base category;
	data _cats_;
		set _cats_ end=_eof_;
		%if &baseval = _FIRST_ | &baseval = LOW 
			%then %str( if _n_ = 1 then delete;);
		%else %if &baseval = _LAST_ | &baseval = HIGH
			%then %str( if _eof_ then delete;);
		%else %str(if &vari = &baseval then delete;);
	run;
	%end;

data _null_ ;
 set _cats_ nobs = numvals ;

 if _n_ = 1 then do;
	%*-- If there are no non-baseline values - abort macro; 
	call symput('abort',trim( left( put( (numvals=0), best. ) ) ) ) ;
	%*-- Place number of dummies into macro variable num;
	call symput( 'num', trim( left( put( numvals, best. ) ) ) ) ;
	end;

	%*-- Number the values, place in macro variables c##; 
	%if %length(&fmt) gt 0 %then %do;
		call symput ( 'c' || trim ( left ( put ( _n_,     best. ) ) ),
								trim(left(put(&vari,&fmt)) ) );
	%end;
	%else %do;
	call symput ( 'c' || trim ( left ( put ( _n_,     best. ) ) ),
								trim ( left ( &vari ) ) ) ;
	%end;
run ;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

proc datasets library=work nolist; delete _cats_; run;

%******************************************************************;
%* Create list of dummy variables for the j-th input variable;
%******************************************************************;

%if "&name" = "VAL" %then %do ;
	%*-- Names by variable value;
	%let vl&j =; 
	%do k=1 %to &num;
		%if %sysevalf(&sysver  < 7 & %length(&pre&&c&k) > 8) %then %do;
			%put ERROR: Cannot generate names longer than 8 characters;
			%let abort=1;
			%goto DONE;
			%end;
		%let vl&j = &&vl&j  &pre&&c&k;
		%end; ;
%*put vl&j = &&&vl&j;

data &out;
	set &out ;
	
	array __d ( &num ) %do k=1 %to &num ;	&pre&&c&k
							%end ; ;
	%put DUMMY: Creating dummy variables &pre&&c1 .. &pre&&c&num for &vari;
	%end ;

%else %do ;
	%*-- Numeric suffix names;
	%let vl&j =; 
	%do k=1 %to &num; 
		%if %sysevalf(&sysver  < 7 & %length(&pre.&k) > 8) %then %do;
			%put ERROR: Cannot generate names longer than 8 characters;
			%let abort=1;
			%goto endloop;
			%end;
		%let vl&j = &&vl&j  &pre.&k;
		%end; ;
%*put vl&j = &&&vl&j;
run;
	
%******************************************************************;
%* Assign values to the dummy variables for the j-th input variable;
%******************************************************************;
data &out  ( rename = ( %do k=1 %to &num ;
						d&k = &pre.&k
						%end ; ) ) ;
	set &out ;
	%put DUMMY: Creating dummy variables &pre.1 .. &pre.&num;
	array __d ( &num ) d1-d&num ;
	%end ;

	%*---------------------------------------------------------;
   %*   Handle missing values (for V7+ only);
	%*-- (to do this for V6.12 requires separate processing for
	      character and numeric variables);
	%*---------------------------------------------------------;
	%if %sysevalf(&sysver  >= 7) %then %do;
     if missing(&vari) then do;
	  	 do j=1 to &num;
        __d(j)=.;
		  end;
		return;
     end;
	%end;

	%*---------------------------------------------------------;
   %*   Assign values to dummy variables;
	%*---------------------------------------------------------;
	drop j;
	do j = 1 to &num ; /* initialize to 0 */
		__d(j) = 0 ;
	end ;


     %if %length(&fmt) eq 0 %then %do;
     %*-- Case 1:  No format;
        if &vari = "&c1" then __d ( 1 ) = 1 ;  /* create dummies */
        %do i = 2 %to &num ;       
           else if &vari="&&c&i" then __d ( &i ) = 1 ;
        %end;
     %end;

     %else %do;
     %*-- Case 2:  with format;
        if put(&vari,&fmt) = "&c1" then __d ( 1 ) = 1 ;
        %do i = 2 %to &num ;       
           else if put(&vari,&fmt)="&&c&i" then __d ( &i ) = 1;
        %end;
     %end;
run ;

%*-- Find the next variable;

%let j=%eval(&j+1);
%let vari = %scan(&var, &j, %str( ));

%*put End of loop(&i): vari = &vari  pre=&pre;
%endloop:
%end;  /* %do %while */

%done:
%if &abort %then %put ERROR: The DUMMY macro ended abnormally.;
options notes;
%mend dummy ;








*Sample selection following Floyd, Li, and Skinner (2015);
rsubmit;
data comp; set comp.fundq (keep = gvkey datadate fyearq fqtr indfmt datafmt popsrc consol CEQQ CAPXY XRDY AQCY SALEQ
/* 18 21 226 115 108 */
IBQ DVY TSTKQ PRSTKCY SSTKY PSTKQ
ATQ);
where (1985 <= year(datadate) <= 2020) and (indfmt='INDL') and (datafmt='STD') and (popsrc='D') and (consol='C')
and (IBQ ne .) and (DVY ne .) and (ATQ gt 0);
;

lnREV=log(saleq);

if ceqq eq . then ceqq=0;
if capxy eq . then capxy=0;
if xrdy eq . then xrdy=0;
if aqcy eq . then aqcy=0;

year=year(datadate);
qtr=(ceil(month(datadate)/3));
drop indfmt datafmt popsrc consol;
rename dvy=div;
run;

*Get assets, treasury stock, and stock purchase lags;
*rsubmit;
proc sort data=comp; by gvkey datadate; run;
data comp0; set comp; lgvkey=lag(gvkey); l4gvkey=lag4(gvkey); lATQ=lag(ATQ); lTSTKQ=lag(TSTKQ); l4TSTKQ=lag4(TSTKQ); lPRSTKCY=lag(PRSTKCY); run;
data comp1; set comp0;
if gvkey ne lgvkey then lATQ=.; if gvkey ne lgvkey then lTSTKQ=.; if gvkey ne l4gvkey then l4TSTKQ=.; if gvkey ne lgvkey then lPRSTKCY=.;
rep1=TSTKQ-l4TSTKQ; rep2=PRSTKCY-SSTKY;
if fqtr eq 1 then rep3=PRSTKCY-PSTKQ; else if fqtr gt 1 then rep3=PRSTKCY-lPRSTKCY-PSTKQ;
drop lgvkey; run;
data comp2; set comp1;
rep=rep1; if rep1 eq 0 then rep=rep2;
if rep3 gt 0 then repQDUM=1; else repQDUM=0;
run;
data comp3; set comp2; if rep1 lt 0 or rep1 eq . or rep2 lt 0 or rep2 eq . then rep=0;

INVEST=(capxy+xrdy+aqcy)/latq;
PROFIT=IBQ/lATQ;

if div gt 0 then div_dum=1; else div_dum=0;
if rep gt 0 then rep_dum=1; else rep_dum=0;
if div gt 0 or rep gt 0 then pay_dum=1; else pay_dum=0;

run;

*Calculate repurchase frequency;
*rsubmit;
proc sql; create table comp_wFREQ as select distinct
	gvkey, fyearq, sum(repQDUM) as NUMREPS
	from comp3
	group by gvkey, fyearq
	order by gvkey, fyearq;
	quit;
data comp_wFREQ1; set comp_wFREQ;
if NUMREPS gt 0 then repFREQ=1; else repFREQ=0;
if NUMREPS gt 2 then repFREQ=2;
run;
proc sql; create table comp3_wFREQ as select
	a.*, b.repFREQ
	from comp3 as a left join comp_wFREQ1 as b
	on a.gvkey eq b.gvkey and a.fyearq eq b.fyearq
	order by a.gvkey, a.datadate;
	quit;


*Get sic code from names file;
*rsubmit;
proc sql; create table comp4 as select
	a.*, b.sic as sic_char
	from comp3_wFREQ as a left join comp.names as b
	on a.gvkey eq b.gvkey;
	quit;
data comp5; set comp4; sicc=sic_char/1; drop prstkcy lprstkcy pstkq sstky tstkq ltstkq rep1 rep2 rep3 sic_char; run;

*Keep only public firms, i.e., those that merge with CRSP;
*rsubmit;
proc sql; create table cc as select 
	a.*, b.lpermno as permno, b.lpermco as permco
    from comp5 as a left join crsp.ccmxpf_linktable
	(where=(linktype in ('LU', 'LC', 'LD', 'LF', 'LN', 'LO', 'LS', 'LX'))) as b
    on (a.gvkey = b.gvkey) 
	and (b.linkdt <= a.datadate or b.linkdt = .B) 
	and (a.datadate <= b.linkenddt or b.linkenddt = .E)
	where permco ne .;
 	quit;

*Separate industrials and commercial banks;
*rsubmit;
data cc_ind; set cc; where (fyearq ge 1988) and sicc ne .; if 4900 <= sicc <= 4949 then delete; if 6000 <= sicc <= 6999 then delete; run;
data cc_cb; set cc; where (fyearq ge 1988) and sicc eq 6020; run;

/* We are close to Floyd et al. Table 1 (ran with annual data);
*rsubmit;
proc sort data=cc_ind; by year; run;
proc means data=cc_ind n mean; vars div_dum rep_dum pay_dum; by year; run;
*/


*Keep only common stocks;
*rsubmit;
proc sql; create table cc_ind1 as select
	a.*, b.shrcd
	from cc_ind as a left join crsp.msenames as b
	on a.permno eq b.permno
	and b.namedt <= a.datadate <= b.nameendt
	where b.shrcd in (10,11);
quit;

*Get (1) market cap at end of q-1 and (2) return in q+1;
*rsubmit;
data CRSPmonth; set crsp.msf (keep = permco permno cusip date ret retx prc vol shrout);
where (permno ne .) and (1985 <= year(date));
year=year(date);
SIZE=abs(prc)*shrout/1000;
PRC=abs(prc);

if vol eq . then vol=0; if vol < 0 then vol=0;
MVOL=(vol*10)/shrout;
run;

*Retrieve delisting returns;
*rsubmit;
DATA delist; SET crsp.mse; WHERE event = 'DELIST'; KEEP permno date dlstcd dlpdt dlret; run;
PROC SORT data=delist; BY permno date; run;
DATA delist; SET delist; RENAME date = dlstdt; FORMAT dlpdt date9.; run;

*rsubmit;
PROC SQL;
	CREATE TABLE CRSPmonth1 
	AS SELECT a.*, b.dlstdt, b.dlret, b.dlstcd
	FROM CRSPmonth as a LEFT JOIN delist as b
	ON a.permno = b.permno AND year(a.date) eq year(b.dlstdt) and month(a.date) eq month(b.dlstdt);
QUIT;

*rsubmit;
DATA CRSPmonth1; SET CRSPmonth1;
IF NOT MISSING(dlret) THEN ret1 = dlret; 
ELSE ret1 = ret;
IF NOT MISSING(ret1) AND ret1 < -1 THEN ret1 = -0.999999;
DROP dlret ret;
run;
DATA CRSPmonth1; SET CRSPmonth1; RENAME ret1 = ret; run;

*Create end of quarter date for delist events;
*rsubmit;
data CRSPmonth2; set CRSPmonth1; if dlstdt ne . then dateWd=intnx('quarter',dlstdt,1)-1; else dateWd=date;
format dateWd YYMMDDN8.;
gret=(1+ret);
run;

*Get quarterly return;
*rsubmit;
proc expand data = CRSPmonth2 out = mr;
	convert gret = ret1 / transformout=( movprod 1);
	convert gret = ret2 / transformout=( movprod 2);
	convert gret = ret3 / transformout=( movprod 3);
	convert MVOL = VOL / transformout=( movsum 3);
	by permno; id date; 
run;
data mr1; set mr; permno3=lag3(permno);
if dlstdt ne . and intnx('month',dlstdt,0)=intnx('month',date,0) then RET=ret3-1;
if dlstdt ne . and intnx('month',dlstdt,1)=intnx('month',date,0) then RET=ret2-1;
if dlstdt ne . and intnx('month',dlstdt,2)=intnx('month',date,0) then RET=ret1-1;
else RET=ret3-1;
run;
data mr2; set mr1; where month(dateWd) in (3,6,9,12); if permno ne permno3 then RET=.; run;

*Merge returns to other data;
*rsubmit;
proc sql; 
	create table qret1 as select
	a.*, b.RET as RET0
	from cc_ind1 as a left join mr2 as b
	on a.permno eq b.permno and intnx('quarter',a.datadate,-1) = intnx('quarter',b.dateWd,0);

	create table qret2 as select
	a.*, b.RET as RET1, abs(b.PRC) as PRC, b.VOL
	from qret1 as a left join mr2 as b
	on a.permno eq b.permno and intnx('quarter',a.datadate,0) = intnx('quarter',b.dateWd,0);

	create table qret3 as select
	a.*, b.RET as RET2
	from qret2 as a left join mr2 as b
	on a.permno eq b.permno and intnx('quarter',a.datadate,1) = intnx('quarter',b.dateWd,0);

	create table cc_ind2 as select
	a.*, b.RET as RET3
	from qret3 as a left join mr2 as b
	on a.permno eq b.permno and intnx('quarter',a.datadate,2) = intnx('quarter',b.dateWd,0);
quit;


*Sum equity value of various share classes to get equity value of firm;
*rsubmit;
proc sql; create table size1 as select distinct
	permco, date, sum(SIZE) as SIZE
	from CRSPmonth2
	group by permco, date
	order by permco, date;
	quit;
proc sql;
	create table size2 as select
	a.*, b.SIZE as SIZEpermco
	from CRSPmonth2 as a left join size1 as b
	on a.permco eq b.permco and a.date eq b.date
	order by a.permno, a.date;
quit;

*Merge past quarter market cap back to other data;
*rsubmit;
proc sql;
	create table cc_ind3_size as select
	a.*, b.cusip, b.SIZEpermco as SIZE
	from cc_ind2 as a left join size2 as b
	on a.permno eq b.permno and intnx('month',a.datadate,-6) = intnx('month',b.date,0);
quit;

*Merge past quarter market cap back to other data;
*rsubmit;
proc sql;
	create table cc_ind3 as select
	a.*, a.CEQQ/b.SIZE as BTM
	from cc_ind3_size as a left join size2 as b
	on a.permno eq b.permno and intnx('month',a.datadate,0) = intnx('month',b.date,0);
quit;


*Get CEO compensation and tenure from Execucomp;
*rsubmit;
data ec; set execcomp.anncomp (keep= gvkey ticker coname year execid CEOANN BECAMECEO TOTAL_ALT1 TOTAL_ALT1_PCT TDC1 TDC2
SALARY BONUS STOCK_AWARDS_FV STOCK_AWARDS OPTION_AWARDS_FV OPTION_AWARDS OPT_EXER_VAL NONEQ_INCENT PENSION_CHG OTHCOMP
SHRS_VEST_VAL OPT_EXER_VAL OPT_UNEX_EXER_EST_VAL
SHROWN_TOT_PCT
);
where (CEOANN eq "CEO");
TENURE = year - year(BECAMECEO);
lnTENURE = log(1 + year - year(BECAMECEO));
TCOMP = SALARY+BONUS+STOCK_AWARDS_FV+OPTION_AWARDS_FV+NONEQ_INCENT+PENSION_CHG+OTHCOMP;
lnTCOMP = log(SALARY+BONUS+STOCK_AWARDS_FV+OPTION_AWARDS_FV+NONEQ_INCENT+PENSION_CHG+OTHCOMP);
VCOMP = SALARY+BONUS+STOCK_AWARDS+OPTION_AWARDS+NONEQ_INCENT+PENSION_CHG+OTHCOMP;
lnVCOMP = log(VCOMP);
NEQUITY = NONEQ_INCENT+BONUS;
lnBONUS = log(1+NONEQ_INCENT+BONUS);
EQUITY = STOCK_AWARDS+OPTION_AWARDS;
lnEQUITY = log(1+STOCK_AWARDS+OPTION_AWARDS);
OCOMP = SALARY+PENSION_CHG+OTHCOMP;
lnOCOMP = log(1+SALARY+PENSION_CHG+OTHCOMP);
run;
proc sort data=ec; by gvkey execid year; run;


*Merge compensation and Compustat/CRSP data;
*rsubmit;
proc sql; create table cc_ind3_comp as select
	a.*, b.CONAME as cname, b.EXECID, b.BECAMECEO, b.TENURE, b.lnTENURE, b.lnTCOMP, b.TCOMP,
	b.lnBONUS,
	b.BONUS, b.NONEQ_INCENT,
	b.SHROWN_TOT_PCT,
	b.lnEQUITY, b.lnOCOMP, b.NEQUITY, b.EQUITY, b.OCOMP, b.VCOMP, b.lnVCOMP,
	b.SALARY, b.STOCK_AWARDS_FV, b.OPTION_AWARDS_FV, b.PENSION_CHG, b.OTHCOMP
	from cc_ind3 as a left join ec as b
	on a.gvkey eq b.gvkey
	and a.fyearq eq b.year;
	quit;




*Remove penny stocks, extreme return stocks, and stocks missing SIZE, as well as calculate payout;
*rsubmit;
data cc_ind4; set cc_ind3_comp; 
where (PRC ge 1) and (-1 <= RET1 <= 3) and (SIZE ne .);
payoutB=(div+rep)/SIZE;
payoutD=(div)/SIZE;
payoutR=(rep)/SIZE;
run;


******************************************************;
* QUARTERLY  ;
******************************************************;


*Calculate above and below median payout;
*rsubmit;
data paidoutB; set cc_ind4; where payoutB gt 0; run;
proc sort data=paidoutB; by year; run;
proc rank data=paidoutB out=paidoutB1 groups=2; var payoutB; ranks RpayoutB; by year; run;

data paidoutD; set cc_ind4; where payoutD gt 0; run;
proc sort data=paidoutD; by year; run;
proc rank data=paidoutD out=paidoutD1 groups=2; var payoutD; ranks RpayoutD; by year; run;

data paidoutR; set cc_ind4; where payoutR gt 0; run;
proc sort data=paidoutR; by year; run;
proc rank data=paidoutR out=paidoutR1 groups=2; var payoutR; ranks RpayoutR; by year; run;


*Merge back to main data (including no payout obs);
*rsubmit;
proc sql; 
	create table cc_ind5 as select
	a.*, b.RpayoutB+1 as RpayoutB
	from cc_ind4 as a left join paidoutB1 as b
	on a.permno eq b.permno and a.datadate = b.datadate;

	create table cc_ind6 as select
	a.*, b.RpayoutD+1 as RpayoutD
	from cc_ind5 as a left join paidoutD1 as b
	on a.permno eq b.permno and a.datadate = b.datadate;

	create table cc_ind7 as select
	a.*, b.RpayoutR+1 as RpayoutR
	from cc_ind6 as a left join paidoutR1 as b
	on a.permno eq b.permno and a.datadate = b.datadate;
quit;
data cc_ind8; set cc_ind7; 
if RpayoutB eq . then RpayoutB=0; if RpayoutD eq . then RpayoutD=0; if RpayoutR eq . then RpayoutR=0;
yr_qtr=year+(qtr/10);
date=intnx('quarter',datadate,1)-1; format date YYMMDDN8.;
run;
*rsubmit;
proc download data=cc_ind8 out=cc_ind_wrds202212comp; run;

/*
data test; set mydir.cc_ind_wrds202212comp; where repFREQ eq 0; run;
data test; set mydir.cc_ind_wrds202212comp; where RpayoutR eq 0; run;
proc corr data=mydir.cc_ind_wrds202212comp; var repFREQ RpayoutR; run;
*/

*Get portfolio returns;
*rsubmit;
proc sql; create table portB as select distinct
	yr_qtr, date, RpayoutB, n(ret0) as n,
	mean(ret0) as ewret0, sum(ret0*SIZE)/sum(SIZE) as vwret0,
	mean(ret1) as ewret1, sum(ret1*SIZE)/sum(SIZE) as vwret1,
	mean(ret2) as ewret2, sum(ret2*SIZE)/sum(SIZE) as vwret2,
	mean(ret3) as ewret3, sum(ret3*SIZE)/sum(SIZE) as vwret3
	from cc_ind8
	group by yr_qtr, RpayoutB
	order by yr_qtr, RpayoutB;
	quit;
proc download data=portB out=portB202212; run;

proc sql; create table portD as select distinct
	yr_qtr, date, RpayoutD, n(ret0) as n,
	mean(ret0) as ewret0, sum(ret0*SIZE)/sum(SIZE) as vwret0,
	mean(ret1) as ewret1, sum(ret1*SIZE)/sum(SIZE) as vwret1,
	mean(ret2) as ewret2, sum(ret2*SIZE)/sum(SIZE) as vwret2,
	mean(ret3) as ewret3, sum(ret3*SIZE)/sum(SIZE) as vwret3
	from cc_ind8
	group by yr_qtr, RpayoutD
	order by yr_qtr, RpayoutD;
	quit;
proc download data=portD out=portD202212; run;

proc sql; create table portR as select distinct
	yr_qtr, date, RpayoutR, n(ret0) as n,
	mean(ret0) as ewret0, sum(ret0*SIZE)/sum(SIZE) as vwret0,
	mean(ret1) as ewret1, sum(ret1*SIZE)/sum(SIZE) as vwret1,
	mean(ret2) as ewret2, sum(ret2*SIZE)/sum(SIZE) as vwret2,
	mean(ret3) as ewret3, sum(ret3*SIZE)/sum(SIZE) as vwret3
	from cc_ind8
	group by yr_qtr, RpayoutR
	order by yr_qtr, RpayoutR;
	quit;
proc download data=portR out=portR202212; run;

proc sql; create table portRfreq as select distinct
	yr_qtr, date, repFREQ, n(ret0) as n,
	mean(ret0) as ewret0, sum(ret0*SIZE)/sum(SIZE) as vwret0,
	mean(ret1) as ewret1, sum(ret1*SIZE)/sum(SIZE) as vwret1,
	mean(ret2) as ewret2, sum(ret2*SIZE)/sum(SIZE) as vwret2,
	mean(ret3) as ewret3, sum(ret3*SIZE)/sum(SIZE) as vwret3
	from cc_ind8
	group by yr_qtr, repFREQ
	order by yr_qtr, repFREQ;
	quit;
proc download data=portRfreq out=portRfreq202212; run;


******************************************************;
* ANNUAL  ;
******************************************************;

*Aggregate payouts and returns to annual level;

/*
*rsubmit;
proc sql; create table cc_ind4a00 as select distinct
	permno, fyearq as year, mean(SIZE) as SIZE, mean(lnREV) as lnREV, mean(lnVCOMP) as lnVCOMP,
	sum(payoutB) as payoutB, sum(payoutD) as payoutD, sum(payoutR) as payoutR, repFREQ,
	exp(sum(log(1+ret1)))-1 as ret0
	from cc_ind4
	group by permno, fyearq
	order by permno, fyearq;
	quit;
*/
*rsubmit;
data cc_ind4a00; set cc_ind4; where fqtr=4; run;

*Get next year return;
*rsubmit;
proc sort data=cc_ind4a00; by permno descending year; run;
data cc_ind4a0; set cc_ind4a00; leadpermno=lag(permno); leadyear=lag(year); ret1=lag(ret0); run;
data cc_ind4a; set cc_ind4a0; if permno ne leadpermno then ret1=.; if leadyear-year ne 1 then ret1=.; drop leadpermno leadyear; run;
proc sort data=cc_ind4a; by permno year; run;

*Calculate above and below median payout;
*rsubmit;
data paidoutB; set cc_ind4a; where payoutB gt 0; run;
proc sort data=paidoutB; by year; run;
proc rank data=paidoutB out=paidoutB1 groups=2; var payoutB; ranks RpayoutB; by year; run;

data paidoutD; set cc_ind4a; where payoutD gt 0; run;
proc sort data=paidoutD; by year; run;
proc rank data=paidoutD out=paidoutD1 groups=2; var payoutD; ranks RpayoutD; by year; run;

data paidoutR; set cc_ind4a; where payoutR gt 0; run;
proc sort data=paidoutR; by year; run;
proc rank data=paidoutR out=paidoutR1 groups=2; var payoutR; ranks RpayoutR; by year; run;


*Merge back to main data (including no payout obs);
*rsubmit;
proc sql; 
	create table cc_ind5a as select
	a.*, b.RpayoutB+1 as RpayoutB
	from cc_ind4a as a left join paidoutB1 as b
	on a.permno eq b.permno and a.year = b.year;

	create table cc_ind6a as select
	a.*, b.RpayoutD+1 as RpayoutD
	from cc_ind5a as a left join paidoutD1 as b
	on a.permno eq b.permno and a.year = b.year;

	create table cc_ind7a as select
	a.*, b.RpayoutR+1 as RpayoutR
	from cc_ind6a as a left join paidoutR1 as b
	on a.permno eq b.permno and a.year = b.year;
quit;
data cc_ind8a; set cc_ind7a; 
if RpayoutB eq . then RpayoutB=0; if RpayoutD eq . then RpayoutD=0; if RpayoutR eq . then RpayoutR=0;
run;
proc download data=cc_ind8a out=cc_ind_wrds202212compa; run;


*Get portfolio returns;
*rsubmit;
proc sql; create table portBa as select distinct
	year, RpayoutB, n(ret0) as n,
	mean(ret0) as ewret0, sum(ret0*SIZE)/sum(SIZE) as vwret0,
	mean(ret1) as ewret1, sum(ret1*SIZE)/sum(SIZE) as vwret1
	from cc_ind8a
	group by year, RpayoutB
	order by year, RpayoutB;
	quit;
proc download data=portBa out=portB202212a; run;

proc sql; create table portDa as select distinct
	year, RpayoutD, n(ret0) as n,
	mean(ret0) as ewret0, sum(ret0*SIZE)/sum(SIZE) as vwret0,
	mean(ret1) as ewret1, sum(ret1*SIZE)/sum(SIZE) as vwret1
	from cc_ind8a
	group by year, RpayoutD
	order by year, RpayoutD;
	quit;
proc download data=portDa out=portD202212a; run;

proc sql; create table portRa as select distinct
	year, RpayoutR, n(ret0) as n,
	mean(ret0) as ewret0, sum(ret0*SIZE)/sum(SIZE) as vwret0,
	mean(ret1) as ewret1, sum(ret1*SIZE)/sum(SIZE) as vwret1
	from cc_ind8a
	group by year, RpayoutR
	order by year, RpayoutR;
	quit;
proc download data=portRa out=portR202212a; run;

proc sql; create table portRfreqa as select distinct
	year, repFREQ, n(ret0) as n,
	mean(ret0) as ewret0, sum(ret0*SIZE)/sum(SIZE) as vwret0,
	mean(ret1) as ewret1, sum(ret1*SIZE)/sum(SIZE) as vwret1
	from cc_ind8a
	group by year, repFREQ
	order by year, repFREQ;
	quit;
proc download data=portRfreqa out=portRfreq202212a; run;


endrsubmit;





************************************************;
*Descriptives;
************************************************;

proc sort data=cc_ind_wrds202212comp out=cc_ind_wrds; by year; run;
data cc_ind_wrds; set cc_ind_wrds; INVESTnoacq=INVEST-(aqcy/latq); drop INVEST; rename INVESTnoacq=INVEST repFREQ=RpayoutF; run;
data cc_ind_wrds; set cc_ind_wrds; if PROFIT < -1 then PROFIT=-1; if PROFIT > 1 then PROFIT=1; if INVEST < -1 then INVEST=-1; if INVEST > 1 then INVEST=1; run;


*Firms paying out each year;
proc means data=cc_ind_wrds n mean;
vars div_dum rep_dum pay_dum; by year;
output out=desc mean(div_dum)=mean_div_dum mean(rep_dum)=mean_rep_dum mean(pay_dum)=mean_pay_dum;
run;
data desc1; set desc; where year ge 1988;
n=_FREQ_/4;
mean_div_dum=round(mean_div_dum*100,.1); mean_rep_dum=round(mean_rep_dum*100,.1); mean_pay_dum=round(mean_pay_dum*100,.1);
drop _TYPE_ _FREQ_;
run;
proc print data=desc1 noobs; run;

*Firms in each portfolio each year;
%let P=D; /* B, D, or R */
proc sql; create table desc&P. as select distinct yr_qtr, Rpayout&P., sum(n) as n, round(yr_qtr,1) as yr from mydir.port&P.202203 group by yr_qtr, Rpayout&P. order by yr_qtr, Rpayout&P.; quit;
data desc&P.0; set desc&P.; where Rpayout&P.=0 and yr_qtr ge 1988; drop Rpayout&P.; run;
data desc&P.1; set desc&P.; where Rpayout&P.=1 and yr_qtr ge 1988; drop Rpayout&P.; run;
data desc&P.2; set desc&P.; where Rpayout&P.=2 and yr_qtr ge 1988; drop Rpayout&P.; run;
proc sql; 
	create table desc&P._ as select
	a.yr_qtr, a.yr, a.n as n0, b.n as n1
	from desc&P.0 a left join desc&P.1 b
	on a.yr_qtr eq b.yr_qtr;

	create table desc&P._qtr as select
	a.*, b.n as n2
	from desc&P._ a left join desc&P.2 b
	on a.yr_qtr eq b.yr_qtr order by a.yr_qtr;
quit;
proc print data=desc&P._qtr noobs; run;
proc sql; create table desc&P._yr as select distinct
	yr, sum(n0)/4 as n0, sum(n1)/4 as n1, sum(n2)/4 as n2
	from desc&P._qtr
	group by yr
	order by yr;
quit;
proc print data=desc&P._yr noobs; run;


*Get payout and other financial variables to annual level;
proc sql; create table aggothfin as select distinct
	permno, fyearq as year,
	sum(VOL) as VOL, sum(BTM)/4 as BTM, sum(PROFIT) as PROFIT, max(INVEST) as INVEST
	from cc_ind_wrds
	group by permno, fyearq
	order by permno, fyearq;
	quit;

proc sql;
	create table cc_ind_wrdsANN as select
	a.*, a.repFREQ as RpayoutF, log(a.SIZE) as lnSIZE, b.VOL, b.BTM, b.PROFIT, b.INVEST
	from cc_ind_wrds202212compa a left join aggothfin b
	on a.permno eq b.permno and a.year eq b.year;
quit;

*Mean payout yield by group;
%let P=R;
data yield1; set cc_ind_wrdsANN; where year ge 1988; run; /* for quarterly cc_ind_wrds */
proc sort data=yield1; by Rpayout&P.; run;
proc means data=yield1 n mean; vars payout&P.; by Rpayout&P.; run;
data yield1_payout; set yield1; where Rpayout&P. ne 0; run;
proc means data=yield1_payout n mean; vars payout&P.; run;
proc means data=yield1 n mean; vars payout&P.; run;


*Mean payout yield by group by year;
%let P=F;
data yield1; set cc_ind_wrdsANN; where year ge 1988; run; 
proc sort data=yield1; by Rpayout&P. year; run;
proc means data=yield1 n mean; vars payout&P.; by Rpayout&P. year; output out=descY mean(payout&P.)=mean_payout&P.; run; proc print data=descY noobs; run;
data yield1_payout; set yield1; where Rpayout&P. ne 0; run;
proc sort data=yield1_payout; by year; run;
proc means data=yield1_payout n mean; vars payout&P.; by year; output out=descY mean(payout&P.)=mean_payout&P.; run; proc print data=descY noobs; run;
proc sort data=yield1; by year; run;
proc means data=yield1 n mean; vars payout&P.; by year; output out=descY mean(payout&P.)=mean_payout&P.; run; proc print data=descY noobs; run;


*Sum payout yield by group by year in $;
data dollars; set cc_ind_wrds; 
where fqtr=4;
DOLLpayoutB=(payoutB*SIZE)/1000;
DOLLpayoutD=(payoutD*SIZE)/1000;
DOLLpayoutR=(payoutR*SIZE)/1000;
run;

%let P=R;
data yield1; set dollars; where year ge 1988; run; 
proc sort data=yield1; by year; run;
proc means data=yield1 n sum; vars DOLLpayout&P.; by year; output out=descY sum(DOLLpayout&P.)=sum_DOLLpayout&P.; run; proc print data=descY noobs; run;





*Mean other financial variables by group;
%let P=R;
data othfin1; set cc_ind_wrdsANN; where year ge 1988; run;
proc sort data=othfin1; by Rpayout&P.; run;
proc means data=othfin1 n mean median; vars VOL BTM PROFIT INVEST; by Rpayout&P.; run;
data othfin1_payout; set othfin1; where Rpayout&P. ne 0; run;
proc means data=othfin1_payout n mean median; vars VOL BTM PROFIT INVEST; run;
proc means data=othfin1 n mean median; vars VOL BTM PROFIT INVEST; run;

*Mean other financial variables by group by year;
%let P=R;
%let ofin=VOL; /* VOL BTM PROFIT INVEST */
data yield1; set cc_ind_wrdsANN; where year ge 1988; run;
proc sort data=yield1; by Rpayout&P. year; run;
proc means data=yield1 n mean; vars &ofin.; by Rpayout&P. year; output out=descY mean(&ofin.)=mean_&ofin.; run; proc print data=descY noobs; run;
data yield1_payout; set yield1; where Rpayout&P. ne 0; run;
proc sort data=yield1_payout; by year; run;
proc means data=yield1_payout n mean; vars &ofin.; by year; output out=descY mean(&ofin.)=mean_&ofin.; run; proc print data=descY noobs; run;
proc sort data=yield1; by year; run;
proc means data=yield1 n mean; vars &ofin.; by year; output out=descY mean(&ofin.)=mean_&ofin.; run; proc print data=descY noobs; run;


**************;
* CEO Compensation;
**************;

data cc_ind_wrdsANN1; set cc_ind_wrdsANN; where lnVCOMP ne .; VCOMP=exp(lnVCOMP); SIZE=exp(lnSIZE); REV=exp(lnREV); run;
%WT(data=cc_ind_wrdsANN1, out=cc_ind_wrdsANN2, byvar=year, vars= lnVCOMP VCOMP lnSIZE SIZE lnREV REV BTM RET0 PROFIT, type = W, pctl = 1 99, drop= N);

*Panel A;
%dummy( 
   data=cc_ind_wrdsANN2,    /* name of input dataset                  */
   out=cc_ind_wrdsANN3,       /* name of output dataset                 */
   var= year,           /* variable(s) to be dummied              */
   base=_FIRST_,     /* base category                          */
   prefix = D_,     /* prefix for dummy variable names        */
   format =,        /* format used to categorize variable     */
   name  = ,     /* VAL: variable names are D_value        */
   fullrank=1       /* Eliminate dummy for baseline category? */
   );

proc surveyreg data=cc_ind_wrdsANN3; cluster permno;
model lnVCOMP = lnSIZE lnREV BTM RET0 PROFIT;
run;quit;

proc surveyreg data=cc_ind_wrdsANN3; cluster permno;
model lnVCOMP = lnSIZE lnREV BTM RET0 PROFIT D_1-D_15;
run;quit;




*Panel B;

proc sort data=cc_ind_wrdsANN2; by year; run;
proc reg data=cc_ind_wrdsANN2;
model lnVCOMP = lnSIZE lnREV BTM RET0 PROFIT; by year;
output out=cc_ind_wrdsANN_out r=rCOMP p=lnPCOMP stdr=sCOMP;
run;quit;

data cc_ind_wrdsANN_out1; set cc_ind_wrdsANN_out; ECOMP=exp(lnVCOMP)-exp(lnPCOMP+((sCOMP*sCOMP)/2.75)); run;
%WT(data=cc_ind_wrdsANN_out1, out=cc_ind_wrdsANN_out2, byvar=year, vars= ECOMP, type = W, pctl = 1 99, drop= N);

%let P=F;
data othfin1; set cc_ind_wrdsANN_out2; run;
proc sort data=othfin1; by Rpayout&P.; run;
proc means data=othfin1 n mean; vars VCOMP ECOMP SIZE REV BTM RET0 PROFIT; by Rpayout&P.; run;
data othfin1_payout; set othfin1; where Rpayout&P. ne 0; run;
proc means data=othfin1_payout n mean; vars VCOMP ECOMP SIZE REV BTM RET0 PROFIT; run;
proc means data=othfin1 n mean; vars VCOMP ECOMP SIZE REV BTM RET0 PROFIT; run;


%let P=R;
%let ofin=VCOMP; /* VCOMP ECOMP */
data yield1; set cc_ind_wrdsANN_out2; run;
proc sort data=yield1; by Rpayout&P. year; run;
proc means data=yield1 n mean; vars &ofin.; by Rpayout&P. year; output out=descY mean(&ofin.)=mean_&ofin.; run; proc print data=descY noobs; run;
data yield1_payout; set yield1; where Rpayout&P. ne 0; run;
proc sort data=yield1_payout; by year; run;
proc means data=yield1_payout n mean; vars &ofin.; by year; output out=descY mean(&ofin.)=mean_&ofin.; run; proc print data=descY noobs; run;
proc sort data=yield1; by year; run;
proc means data=yield1 n mean; vars &ofin.; by year; output out=descY mean(&ofin.)=mean_&ofin.; run; proc print data=descY noobs; run;




**************;
* Portfolio Returns;
**************;


*Get Fama-French factor returns;
PROC IMPORT OUT=WORK.FF
DATAFILE="C:\Users\nmg75\Dropbox\Research\017 - GKV - Repurchases\Data\FF Factors\FF6Factors.xlsx"
DBMS=EXCEL REPLACE;GETNAMES=YES;MIXED=YES;SCANTEXT=YES;USEDATE=YES;SCANTIME=YES;
run;
data FF1; set FF;
year=round(date/100,1);
month=round(((date/100)-(round(date/100,1)))*100);
gMKT_RF=1+MKT_RF;gSMB=1+SMB;gHML=1+HML;gUMD=1+UMD;gRMW=1+RMW;gCMA=1+CMA;
drop MKT_RF SMB HML UMD RMW CMA;
run;




******************************************************;
* QUARTERLY  ;
******************************************************;

*Aggregate FF to quarterly return;
proc expand data = FF1 out = FFqtr;
convert gMKT_RF = MKT_RF / transformout=( movprod 3 -1);
convert gSMB = SMB / transformout=( movprod 3 -1);
convert gHML = HML / transformout=( movprod 3 -1);
convert gUMD = UMD / transformout=( movprod 3 -1);
convert gRMW = RMW / transformout=( movprod 3 -1);
convert gCMA = CMA / transformout=( movprod 3 -1);
id date; 
run;

*Beginning and end date: 1988-2021, 2001-2011, 2011-2021;
%let begyr=2001;
%let endyr=2011;

*Create date variable for each of four quarters of interest;
data portRdate; set mydir.portR202212; where (&begyr. <= yr_qtr < &endyr.); 
date0=intnx('quarter',date,0)-1; format date0 YYMMDDN8.;
date1=intnx('quarter',date,1)-1; format date1 YYMMDDN8.;
date2=intnx('quarter',date,2)-1; format date2 YYMMDDN8.;
date3=intnx('quarter',date,3)-1; format date3 YYMMDDN8.;
run;
data portRfreqdate; set mydir.portRfreq202212; where (&begyr. <= yr_qtr < &endyr.); 
date0=intnx('quarter',date,0)-1; format date0 YYMMDDN8.;
date1=intnx('quarter',date,1)-1; format date1 YYMMDDN8.;
date2=intnx('quarter',date,2)-1; format date2 YYMMDDN8.;
date3=intnx('quarter',date,3)-1; format date3 YYMMDDN8.;
run;


*date0, date1, date2, date3;
%let q=3;

*Merge FF factor returns into portfolio returns;
proc sql; 
	create table portRFF as select
	a.*, b.date as dateFF, a.ewret&q.-b.RF as ewret&q._RF, a.vwret&q.-b.RF as vwret&q._RF,
	b.RF, b.MKT_RF, b.SMB, b.HML, b.UMD, b.RMW, b.CMA
	from portRdate as a left join FFqtr as b
	on year(a.date&q.) eq b.year and month(a.date&q.) eq b.month;

	create table portRfreqFF as select
	a.*, b.date as dateFF, a.ewret&q.-b.RF as ewret&q._RF, a.vwret&q.-b.RF as vwret&q._RF,
	b.RF, b.MKT_RF, b.SMB, b.HML, b.UMD, b.RMW, b.CMA
	from portRfreqdate as a left join FFqtr as b
	on year(a.date&q.) eq b.year and month(a.date&q.) eq b.month;
quit;

*Create hedge returns: long above median payout and short no payout;
data hedgeR_N; set portRFF; where RpayoutR=0; run; data hedgeR_A; set portRFF; where RpayoutR=2; run;
data hedgeRfreq_N; set portRfreqFF; where repFREQ=0; run; data hedgeRfreq_A; set portRfreqFF; where repFREQ=2; run;
proc sql; 
	create table hedgeR as select
	a.date, a.ewret&q.-b.ewret&q. as ewHret&q., a.vwret&q.-b.vwret&q. as vwHret&q.  
	from hedgeR_A a left join hedgeR_N b
	on a.date eq b.date order by a.date;

	create table hedgeRfreq as select
	a.date, a.ewret&q.-b.ewret&q. as ewHret&q., a.vwret&q.-b.vwret&q. as vwHret&q.  
	from hedgeRfreq_A a left join hedgeRfreq_N b
	on a.date eq b.date order by a.date;
quit;
proc sql; 
	create table portRFF1 as select
	a.*, b.ewHret&q., b.vwHret&q.  
	from portRFF a left join hedgeR b
	on a.date eq b.date order by a.date;

	create table portRfreqFF1 as select
	a.*, b.ewHret&q., b.vwHret&q.  
	from portRfreqFF a left join hedgeRfreq b
	on a.date eq b.date order by a.date;
quit;


************************************************;
*Average returns and excess returns;
************************************************;

*Only repurchases;
proc sort data=portRFF1; by RpayoutR; run;
proc reg data=portRFF1; by RpayoutR;
/*model ewret&q._RF =;*/
/*model ewret&q._RF = MKT_RF SMB HML UMD RMW CMA;*/
/*model vwret&q._RF =;*/
model vwret&q._RF = MKT_RF SMB HML UMD RMW CMA;
ods output parameterestimates = parmsR;
run;quit;
data parmsR1; set parmsR; where Variable eq 'Intercept'; Estimate=round(Estimate*100,.01); run;
data HportRFF1; set portRFF1; where RpayoutR=0; drop RpayoutR; run;
proc reg data=HportRFF1;
/*model ewHret&q. =;*/
/*model ewHret&q. = MKT_RF SMB HML UMD RMW CMA;*/
/*model vwHret&q. =;*/
model vwHret&q. = MKT_RF SMB HML UMD RMW CMA;
ods output parameterestimates = parmsR;
run;quit;
data parmsR1hedge; set parmsR; where Variable eq 'Intercept'; Estimate=round(Estimate*100,.01); run;

*Only repurchases - FREQUENCY;
proc sort data=portRfreqFF1; by repFREQ; run;
proc reg data=portRfreqFF1; by repFREQ;
/*model ewret&q._RF =;*/
/*model ewret&q._RF = MKT_RF SMB HML UMD RMW CMA;*/
/*model vwret&q._RF =;*/
model vwret&q._RF = MKT_RF SMB HML UMD RMW CMA;
ods output parameterestimates = parmsRfreq;
run;quit;
data parmsRfreq1; set parmsRfreq; where Variable eq 'Intercept'; Estimate=round(Estimate*100,.01); run;
data HportRfreqFF1; set portRfreqFF1; where repFREQ=0; drop repFREQ; run;
proc reg data=HportRfreqFF1;
/*model ewHret&q. =;*/
/*model ewHret&q. = MKT_RF SMB HML UMD RMW CMA;*/
/*model vwHret&q. =;*/
model vwHret&q. = MKT_RF SMB HML UMD RMW CMA;
ods output parameterestimates = parmsRfreq;
run;quit;
data parmsRfreq1hedge; set parmsRfreq; where Variable eq 'Intercept'; Estimate=round(Estimate*100,.01); run;


proc print data=parmsR1; run;
proc print data=parmsR1hedge; run;
proc print data=parmsRfreq1; run;
proc print data=parmsRfreq1hedge; run;



/*
data ftestR; set parmsR1; where RpayoutR ne 1; run;
proc sort data=ftestR; by model; run;
data ftestR; set ftestR; lEstimate=lag(Estimate); lStderr=lag(Stderr); run;
data ftestR; set ftestR; where RpayoutR eq 2; Fstat=abs(Estimate-lEstimate)/(sqrt(Stderr*Stderr+lStderr*lStderr)); run;
data ftestR; set ftestR; probF=1-probf(Fstat,130,130); run;
proc print data=ftestR; var Model Fstat probF; run;
*/




******************************************************;
* ANNUAL  ;
******************************************************;

*Aggregate FF to annual return;
proc expand data = FF1 out = FFann1;
convert gMKT_RF = MKT_RF / transformout=( movprod 12 -1);
convert gSMB = SMB / transformout=( movprod 12 -1);
convert gHML = HML / transformout=( movprod 12 -1);
convert gUMD = UMD / transformout=( movprod 12 -1);
convert gRMW = RMW / transformout=( movprod 12 -1);
convert gCMA = CMA / transformout=( movprod 12 -1);
id date; 
run;

data FFann2; set FFann1; where month eq 6; run;

*Beginning and end date: 1988-2020, 2000-2011, 2010-2021;
%let begyr=2000;
%let endyr=2010;

*Create date variable for each of four quarters of interest;
data portRdate; set mydir.portR202212a; where (&begyr. <= year < &endyr.);run;
data portRfreqdate; set mydir.portRfreq202212a; where (&begyr. <= year < &endyr.);run;

*date0, date1;
%let q=1;

*Merge FF factor returns into portfolio returns;
proc sql; 
	create table portRFF as select
	a.*, b.date as dateFF, a.ewret&q.-b.RF as ewret&q._RF, a.vwret&q.-b.RF as vwret&q._RF,
	b.RF, b.MKT_RF, b.SMB, b.HML, b.UMD, b.RMW, b.CMA
	from portRdate as a left join FFann2 as b
	on a.year+&q.+1 eq b.year;

	create table portRfreqFF as select
	a.*, b.date as dateFF, a.ewret&q.-b.RF as ewret&q._RF, a.vwret&q.-b.RF as vwret&q._RF,
	b.RF, b.MKT_RF, b.SMB, b.HML, b.UMD, b.RMW, b.CMA
	from portRfreqdate as a left join FFann2 as b
	on a.year+&q.+1 eq b.year;
quit;

*Create hedge returns: long above median payout and short no payout;
data hedgeR_N; set portRFF; where RpayoutR=0; run; data hedgeR_A; set portRFF; where RpayoutR=2; run;
data hedgeRfreq_N; set portRfreqFF; where repFREQ=0; run; data hedgeRfreq_A; set portRfreqFF; where repFREQ=2; run;
proc sql; 
	create table hedgeR as select
	a.year, a.ewret&q.-b.ewret&q. as ewHret&q., a.vwret&q.-b.vwret&q. as vwHret&q.  
	from hedgeR_A a left join hedgeR_N b
	on a.year eq b.year order by a.year;

	create table hedgeRfreq as select
	a.year, a.ewret&q.-b.ewret&q. as ewHret&q., a.vwret&q.-b.vwret&q. as vwHret&q.  
	from hedgeRfreq_A a left join hedgeRfreq_N b
	on a.year eq b.year order by a.year;
quit;
proc sql; 
	create table portRFF1 as select
	a.*, b.ewHret&q., b.vwHret&q.  
	from portRFF a left join hedgeR b
	on a.year eq b.year order by a.year;

	create table portRfreqFF1 as select
	a.*, b.ewHret&q., b.vwHret&q.  
	from portRfreqFF a left join hedgeRfreq b
	on a.year eq b.year order by a.year;
quit;


************************************************;
*Average returns and excess returns;
************************************************;

*Only repurchases;
proc sort data=portRFF1; by RpayoutR; run;
proc reg data=portRFF1; by RpayoutR;
/*model ewret&q._RF =;*/
/*model ewret&q._RF = MKT_RF SMB HML UMD RMW CMA;*/
/*model vwret&q._RF =;*/
model vwret&q._RF = MKT_RF SMB HML UMD RMW CMA;
ods output parameterestimates = parmsR;
run;quit;
data parmsR1; set parmsR; where Variable eq 'Intercept'; Estimate=round(Estimate*100,.01); run;
data HportRFF1; set portRFF1; where RpayoutR=0; drop RpayoutR; run;
proc reg data=HportRFF1;
/*model ewHret&q. =;*/
/*model ewHret&q. = MKT_RF SMB HML UMD RMW CMA;*/
/*model vwHret&q. =;*/
model vwHret&q. = MKT_RF SMB HML UMD RMW CMA;
ods output parameterestimates = parmsR;
run;quit;
data parmsR1hedge; set parmsR; where Variable eq 'Intercept'; Estimate=round(Estimate*100,.01); run;


*Only repurchases - FREQUENCY;
proc sort data=portRfreqFF1; by repFREQ; run;
proc reg data=portRfreqFF1; by repFREQ;
/*model ewret&q._RF =;*/
/*model ewret&q._RF = MKT_RF SMB HML UMD RMW CMA;*/
/*model vwret&q._RF =;*/
model vwret&q._RF = MKT_RF SMB HML UMD RMW CMA;
ods output parameterestimates = parmsRfreq;
run;quit;
data parmsRfreq1; set parmsRfreq; where Variable eq 'Intercept'; Estimate=round(Estimate*100,.01); run;
data HportRfreqFF1; set portRfreqFF1; where repFREQ=0; drop repFREQ; run;
proc reg data=HportRfreqFF1;
/*model ewHret&q. =;*/
/*model ewHret&q. = MKT_RF SMB HML UMD RMW CMA;*/
/*model vwHret&q. =;*/
model vwHret&q. = MKT_RF SMB HML UMD RMW CMA;
ods output parameterestimates = parmsRfreq;
run;quit;
data parmsRfreq1hedge; set parmsRfreq; where Variable eq 'Intercept'; Estimate=round(Estimate*100,.01); run;



proc print data=parmsR1; run;
proc print data=parmsR1hedge; run;
proc print data=parmsRfreq1; run;
proc print data=parmsRfreq1hedge; run;



/*
data ftestR; set parmsR1; where RpayoutR ne 1; run;
proc sort data=ftestR; by model; run;
data ftestR; set ftestR; lEstimate=lag(Estimate); lStderr=lag(Stderr); run;
data ftestR; set ftestR; where RpayoutR eq 2; Fstat=abs(Estimate-lEstimate)/(sqrt(Stderr*Stderr+lStderr*lStderr)); run;
data ftestR; set ftestR; probF=1-probf(Fstat,130,130); run;
proc print data=ftestR; var Model Fstat probF; run;
*/





/* Good vs. Bad Repurchase Anecdotes */

data anec1; set cc_ind_wrds; where year gt 2015 and RpayoutR eq 2 and size gt 10000; run;


proc sort data=anec1; by ret1; run;

data anec_bad; set anec1; where profit lt 0.01 and invest lt 0.02; run;
