 /*-------------------------------------------------------------------*/
 /*                 Bealls Eligibility Developement                   */
 /*                    		     by PSCC                      	      */
 /*-------------------------------------------------------------------*/
 /*                                                                   */
 /* This code is developed on Dev environment BLSDEV01.VSP.SAS.COM    */
 /* Code includes merch Id from Attr_Ml where eligibility start       */
 /* date , end date, start year and end year is null. this means that */
 /* it take in to account merch members whose either one 	 	      */
 /* charateristics are defined.                                       */
 /*      				       										  */
 /*                   	     					 					  */
 /*-------------------------------------------------------------------*/
 /* Date last updated: 27 June 2012 15:19                             */
 /*-------------------------------------------------------------------*/
 /* Questions or problem reports concerning this material may be      */
 /* addressed to the author: sinume(umesh.mahajan@sas.com), PSCC.     */
 /*-------------------------------------------------------------------*/


/*=STEP:001 START================================================ Code that include Header Files ===================================================*/
	%global this_program_name previous_program_name;
	%let previous_program_name=&this_program_name;
	%let this_program_name=mdin_bealls_eligibility;
	%include "%sysget(MDI_SASCODE_DIR)/mdi_common.sas"; /* forward-slash works here on both Windows and Unix */

	%timestamp(b,mdin_bealls_eligibility)
	%db_libname(libname=datamgr,schema=datamgr,user=madmax)
	%db_libname(libname=maxapp, schema=maxapp, user=madmax)
	%db_libname(libname=maxdata,schema=maxdata,user=madmax)
	%db_libname(libname=maxdataw,schema=maxdata,user=maxdata)
	
	/* include Eligibility load specific parameter file */
	%include "&mdi_noncore_config_directory/mdi_noncore_eligibility_config.sas";
/*=STEP:001 END=====================================================================================================================================*/


/*=STEP:002 START=======================Get Distinct Merch Member from Attr_ML table for whom eligibilty is defiend in Attr_ML table================*/
	* options mlogic mprint symbolgen;
        options mprint;
	proc sql;
	create table eligible_merch as
	select distinct merch_ID,merch_level,location_ID,location_level, ELIGIBLE_START_DATE, ELIGIBLE_END_DATE, START_YEAR, END_YEAR
	from maxdata.attr_ml
	where MERCH_LEVEL <=16 ;
	/*where eligible_start_date is not null;*/
	quit;
/*=STEP:002 END====================================================================================================================================*/


/*=STEP:003 START=========================This query Gets time members for next 30 weeks from current week=========================================*/
	data lv5time;
	set maxapp.lv5time;
	n=_n_;
	run;

	proc sql;
	create table Lv5time_30wk as
		select * from lv5time
				 where n > (select n
							from lv5time,maxapp.mmax_config
							where lv5time.LV5TIME_ID = mmax_config.current_period and lv5time.cycle_id = mmax_config.CURRENT_CYCLE)
				   and n <= ((select n from lv5time,maxapp.mmax_config
							  where lv5time.LV5TIME_ID = mmax_config.current_period and lv5time.cycle_id = mmax_config.CURRENT_CYCLE)+30);
	quit;
/*=STEP:003 END===================================================================================================================================*/


/*=STEP:004 START========Extract Eligible start month and eligible start week, Eligible end month and week for each merch from Attr_ML table======*/
	/*NOTE: Code Change to for differenct scenarios of startdate and start Year*/
	data attr_ML ;
	set eligible_merch (keep=merch_id MERCH_LEVEL LOCATION_ID LOCATION_LEVEL ELIGIBLE_START_DATE ELIGIBLE_END_DATE START_YEAR END_YEAR );
		ELIGIBLE_ST_MN = input(substr(trim(put(ELIGIBLE_START_DATE,z4.)),1,2),2.);
		ELIGIBLE_ST_WK = input(substr(trim(put(ELIGIBLE_START_DATE,z4.)),3,2),2.);
		ELIGIBLE_EN_MN = input(substr(trim(put(ELIGIBLE_END_DATE,z4.)),1,2),2.) ;
		ELIGIBLE_EN_WK = input(substr(trim(put(ELIGIBLE_END_DATE,z4.)),3,2),2.) ;
	where ELIGIBLE_START_DATE not eq 9999 and  ELIGIBLE_END_DATE not eq 9998 and (ELIGIBLE_START_DATE ne . or ELIGIBLE_END_DATE ne . or START_YEAR ne . or END_YEAR ne . );
	run;

        proc sql;
        select count(*) into :attr_ml_cnt from attr_ML;
        select count(*) into :attr_ml_invalid_cnt from eligible_merch
        where ELIGIBLE_START_DATE not eq 9999 and  ELIGIBLE_END_DATE not eq 9998 and (ELIGIBLE_START_DATE eq . and ELIGIBLE_END_DATE eq . and  START_YEAR eq . and END_YEAR eq . );
        quit;
/*
        %logger(m,info, %bquote([:s3a1 mdin_bealls_eligibility] Read &attr_ml_cnt records from maxdata.attr_ml table with eligibility defiend by either start end date or start end year ))
        %logger(m,warn, %bquote([:s3w1 mdin_bealls_eligibility] Rejected &attr_ml_invalid_cnt records from maxdata.attr_ml table with eligibility start and end dates and year missing))
*/

	proc sql;
	create table eligibility_MLT as
		select  a.merch_ID, a.merch_level, a.location_ID, a.location_level, b.LV5TIME_LKUP_ID as time_ID, 51 as time_level
		from attr_ML a, Lv5time_30wk b;
	quit;
/*=STEP:004 END====================================================================================================================================*/



/*=STEP:005.1 START=============================Set eligibility Start, end date and start and end year if they are null============================*/
	/*Get start year*/
		data _null_;
		set Lv5time_30wk (firstobs=1 obs=1 keep=LV5TIME_ID cycle_ID LV4TIME_LKUP_ID);
		call symput('cur_st_weekID',LV5TIME_ID);
		call symput('cur_st_year',cycle_ID);
		call symput('cur_st_monthID',LV4TIME_LKUP_ID);
		run;
	/*Get End year*/
		data _null_;
		set Lv5time_30wk (keep=LV5TIME_ID cycle_ID LV4TIME_LKUP_ID ) end=m;
		if m ;
		call symput('cur_end_weekID',LV5TIME_ID);
		call symput('cur_end_year',cycle_ID);
		call symput('cur_end_monthID',LV4TIME_LKUP_ID);
		run;
	/* Get start week # in that start month*/
		data test;
		set maxapp.Lv5time(where= (LV4TIME_LKUP_ID=&cur_st_monthID));
		n=_n_;
		if LV5TIME_ID=&cur_st_weekID then
			do;
				call symput('cur_st_week',n);
			end;
		run;

	/* Get End week # in that End month*/
		data test;
		set maxapp.Lv5time(where= (LV4TIME_LKUP_ID=&cur_end_monthID));
		n=_n_;
		if LV5TIME_ID=&cur_end_weekID then
			do;
				call symput('cur_end_week',n);
			end;
		run;

	%put cur_st_week=&cur_st_week;
	%put cur_st_year=&cur_st_year;
	%put cur_st_monthID=&cur_st_monthID;
	%put cur_end_week=&cur_end_week;
	%put cur_end_year=&cur_end_year;
	%put cur_end_monthID=&cur_end_monthID;

	/* Get Start month # in that start LV4TIME_LKUP_ID*/
	proc sql;
	select LV4TIME_ID into :cur_st_month
	from maxapp.lv4time
	where LV4TIME_LKUP_ID=&cur_st_monthID;
	quit;

	/* Get End month # in that end LV4TIME_LKUP_ID*/
	proc sql;
	select LV4TIME_ID into :cur_end_month
	from maxapp.lv4time
	where LV4TIME_LKUP_ID=&cur_end_monthID;
	quit;

	%put cur_st_monthID=&cur_st_month;
	%put cur_end_monthID=&cur_end_month;

	data attr_Ml;
	set attr_ML;
	if ELIGIBLE_START_DATE eq . then
		do;
			ELIGIBLE_ST_MN=&cur_st_month;
			ELIGIBLE_ST_WK=&cur_st_week ;
		end;

	if ELIGIBLE_END_DATE eq . then
		do;
			ELIGIBLE_EN_MN=&cur_end_month;
			ELIGIBLE_EN_WK=&cur_end_week;
		end;
	if START_YEAR eq . then do ; START_YEAR=&cur_st_year; end;

	if END_YEAR eq . then do; END_YEAR=&cur_end_year; end;
	run;

/*=STEP:005.1 END=================================================================================================================================*/

/*=STEP:005.2 START======= CREATE SMALLER DATASETS ================================================================================================*/
/* create smaller datasets */

%macro smallerds;
	%do cnt=1 %to &max_elig_parallel_threads;
	    data sasdata.attr_ml_thread_&cnt;
	       set attr_ml;
		   if (mod(_N_,&max_elig_parallel_threads)=%eval(&cnt -1));
	    run;
		data sasdata.attr_ml_&cnt;
		   set attr_ml;
		run;
		data sasdata.Lv5time_30wk_&cnt;
		   set Lv5time_30wk;
		run;
		data sasdata.lv5time_&cnt.;
		   set lv5time;
		run;
		data sasdata.eligibility_MLT_&cnt.; 
      		   if 0 then set eligibility_MLT;
		   stop;
		run;	
	%end;
%mend smallerds;
%smallerds;

/*=STEP:005.2 END======= CREATE SMALLER DATASETS ================================================================================================*/

/*=STEP:005 START=======Delete the time mebers where merch_ID is eligible whichis defined in Attr_ML table=========================================*/
%macro _delete_eligible_time_ids;
%global waitlist; %let waitlist=;
%do icnt=1 %to &max_elig_parallel_threads;

	options autosignon =yes;
	rsubmit elig&icnt connectwait=no connectpersist=yes signonwait=no inheritlib=(work=remote) sascmd="&mdi_sascmd -sasuser WORK -set PATH %bquote(&local_path)";
		options source2 fullstimer;
	endrsubmit;
	%syslput thread_no=&icnt   /remote=elig&icnt;
	rsubmit elig&icnt connectwait=no connectpersist=yes;

		%include "%sysget(MDI_SASCODE_DIR)/mdi_common.sas"; /* forward-slash works here on both Windows and Unix */
		%db_libname(libname=datamgr,schema=datamgr,user=madmax)
		%db_libname(libname=maxapp, schema=maxapp, user=madmax)
		%db_libname(libname=maxdata,schema=maxdata,user=madmax)
		%db_libname(libname=maxdataw,schema=maxdata,user=maxdata)
		%include noncore(mdin_delete_eligible_time_ids.sas);
		options nomprint nosource;
		data _null_;
			set sasdata.attr_ml_thread_&thread_no. ;
			call execute('%mdin_delete_eligible_time_ids('||merch_ID||','||merch_level||','||location_ID||','||location_level||','||"&thread_no"||')');
		run;

		proc sort data=sasdata.eligibility_mlt_&thread_no.;
			by merch_level merch_id location_level location_id time_id;
		run;
		
	endrsubmit;
	%let waitlist=&waitlist elig&icnt;

%end;	

%mend _delete_eligible_time_ids;
%_delete_eligible_time_ids

%macro waitforall;
/* wait for all threads to finish */
waitfor _ALL_ &waitlist;
   %do s=1 %to &max_elig_parallel_threads;
      rget    elig&s;
      signoff elig&s;
   %end;
%mend waitforall;
%waitforall
options mprint source;

%macro delete_from_MLT;
proc sort data=eligibility_mlt; 
by merch_level merch_id location_level location_id time_id;
run;

%do thread=1 %to &max_elig_parallel_threads;
/*
	proc sort data=sasdata.eligibility_mlt_&thread.;
		by merch_level merch_id location_level location_id time_id;
	run;
*/
	data eligibility_mlt;
		merge eligibility_mlt(in=in1) sasdata.eligibility_mlt_&thread.(in=in2);
		by merch_level merch_id location_level location_id time_id;
		if in2 then delete;
	run;

%end;
%mend delete_from_MLT;
%delete_from_MLT


/*=STEP:005 END==================================================================================================================================*/

/*=STEP:006 START==============Store eligibility flag for merch memebrs with start date 9999=====================================================*/
	data eligibility_ML_9999;
	set eligible_merch (keep=merch_id MERCH_LEVEL LOCATION_ID LOCATION_LEVEL ELIGIBLE_START_DATE ELIGIBLE_END_DATE );
		where ELIGIBLE_START_DATE  eq 9999 ;
	run;

        proc sql;
        select count(*) into :elig_ml_cnt from eligibility_ML_9999;
        quit;

        %logger(m,info, %bquote([:s3a2 mdin_bealls_eligibility] Read &elig_ml_cnt records from maxdata.attr_ml table with eligibility start data as 9999))

	proc sql;
	create table eligibility_MLT_9999 as
		select a.merch_id, a.MERCH_LEVEL, a.LOCATION_ID, a.LOCATION_LEVEL,
				b.LV5TIME_LKUP_ID as time_ID, 51 as time_level
		from eligibility_ML_9999 a, Lv5time_30wk b;
	quit;
/*=STEP:006 END===================================================================================================================================*/


/*=STEP:007 START================Code to get Parent of each merch member==========================================================================*/
	proc sql;
	create table distinct_eligible_mlt as
		select distinct merch_id, MERCH_LEVEL, LOCATION_ID, LOCATION_LEVEL
		from eligibility_mlt;
	quit;

	data parent_eligibility;
	input child_merch_ID
	Parent_merch_ID
	parent_Merch_level
	Location_ID
	location_level;
	datalines;
	;
	run;


	/* Get Merhcnadise level */
	data distinct_eligible_mlt;
	set distinct_eligible_mlt;
	mplevel=merch_level-10;
	run;


	%macro get_merch_parent_members(merch_ID,Merch_level,Location_ID,location_level,level);
		%if &level ne 2
			%then %do ;
				%do n=2 %to %eval(&level-1);
				proc sql;
				create table mp&n as
					select distinct &merch_ID as child_merch_ID, lv&n.ctree_id as Parent_merch_ID, %eval(&n+10) as parent_Merch_level, &Location_ID as Location_ID,
					&location_level as location_level
					from maxdata.lv&level.ctree
					where lv&level.ctree_id=&merch_ID ;
				quit;
				%end;
			/* TODO: In set operator name of datsets need to be changes it should be dynamic*/
			data Parent_eligibility;
				set Parent_eligibility mp2-mp%eval(&level-1);
				label Parent_merch_ID='Parent_merch_ID';
			run;
		%end;
	%mend;

	data _null_;
	set distinct_eligible_mlt;
		call execute('%get_merch_parent_members('||merch_ID||','||merch_level||','||location_ID||','||location_level||','||mplevel||')');
	run;
/*=STEP:007 END=====================================================================================================================================*/


/*=STEP:008 START===================Alter parent MLT info who are defined in eligiblity_mlt_9999====================================================*/
	proc sql;
	create table parent_ML as
		select distinct A.parent_merch_ID,A.parent_merch_level,A.LOCATION_ID, A.LOCATION_LEVEL informat=7.,a.child_merch_id
		from Parent_eligibility A, eligibility_MLT_9999 B
		where a.parent_merch_ID=b.merch_ID and B.LOCATION_ID=A.LOCATION_ID and B.LOCATION_LEVEL=A.LOCATION_LEVEL;
	quit;

	/* Get parent's MLT */
	proc sql;
	Create table Parent_MLT as
		select distinct B.Parent_merch_ID,B.parent_Merch_level, B.LOCATION_ID, B.LOCATION_LEVEL, A.time_ID, A.time_level
		from eligibility_MLT A,parent_ML B
		where B.child_merch_ID=A.merch_id and B.LOCATION_ID=B.LOCATION_ID and B.LOCATION_LEVEL=A.LOCATION_LEVEL ;
	quit;

        /* todo vigovi start */
        Proc sort data = eligibility_mlt_9999 ;
           by merch_id merch_level location_id;
        run;

        Proc sort data = parent_ml out=parent_ml_sorted (rename=(parent_merch_id=merch_id parent_merch_level=merch_level));
           by parent_merch_id parent_merch_level location_id;
        run;

        data ex_p_eligibility_mlt_9999;
           merge eligibility_mlt_9999 ( in = a) parent_ml_sorted (in = b keep=merch_id merch_level location_id);
           by merch_id merch_level location_id;
           if a and not b;
        run;

        /* todo vigovi - Proc sql below is returning erronous results in some cases, therefore replaced with proc sort/data merge steps above */
	/* Exclude Parent's MLT from eligibility_mlt_9999*/
	/*
	proc sql;
	create table ex_p_eligibility_mlt_9999 as
		select a.*
		from eligibility_mlt_9999 a
		where a.merch_ID not in(select b.parent_merch_id from parent_ml b)
		and a.merch_level not in(select b.parent_merch_level from parent_ml b)
		and a.location_id not in(select b.location_id from parent_ml b);
	quit;
	*/
        /* todo vigovi end */

	/* Append Parent MLT to eligibility_mlt_9999*/
	data eligibility_mlt_9999;
	set ex_p_eligibility_mlt_9999 Parent_MLT(rename=(parent_merch_id=merch_id parent_merch_level=merch_level));
	run;
/*=STEP:008 END==================================================================================================================================*/


/*=STEP:009 START============================Combile MLT for 9999 and merch where eligibility defiend for specific period==========================*/
	data eligibility_all;
	set eligibility_MLT_9999 eligibility_MLT;
	eligibility_flag=1;
	run;

	proc sql;
	create table distinct_eligible_mltAll as
		select distinct merch_id, MERCH_LEVEL, LOCATION_ID, LOCATION_LEVEL
		from eligibility_all;
	quit;
/*=STEP:009 END==================================================================================================================================*/



/*=STEP:010 START===============Code to get children on each merch member=========================================================================*/

	data lv6ctree;
	set maxdata.lv6ctree(keep=lv2ctree_id lv3ctree_id lv4ctree_id lv5ctree_id lv6ctree_id ) ;
	run;

	proc sql;
	create table distinct_merch as
		select distinct merch_ID,Merch_level,(merch_level-10) as mlevel label='mlevel'
		from distinct_eligible_mltAll;
	quit;

	data child_eligibility;
	input Parent_merch=;
	input Parent_Merch_level=;
	input child_merch_ID=;
	input child_Merch_level=;
	datalines;
	;
	run;

	%macro get_merch_child_members(merch_ID,Merch_level,i);
		%do n=&i %to 6;

			proc sql;
			create table m&n as
				select distinct &merch_ID as Parent_merch, &i as Parent_Merch_level, lv&n.ctree_id as child_merch_ID, &n as child_Merch_level
				from lv6ctree
				where lv&i.ctree_id=&merch_ID ;
			quit;

		%end;
		data child_eligibility;
			set child_eligibility m&i-m6;
			label merch_ID='merch_ID';
		run;
	%mend;
	/* run the macro for each merch_ID fro which eligibility is defend or 9999*/
	data _null_;
	set distinct_merch;
		call execute('%get_merch_child_members('||merch_ID||','||merch_level||','||mlevel||')');
	run;
/*=STEP:010 END======================================================================================================================================*/



/*=STEP:011 START============================Append time Id's to merch loc child combination from parent==============================================*/

	proc sql;
	create table meligibility as
		select distinct A.child_merch_ID as merch_ID label="merch_ID", A.child_Merch_level as Merch_level label="Merch_level",
			B.LOCATION_ID, B.LOCATION_LEVEL, B.time_ID label="time_ID",B.time_level,B.eligibility_flag
		from child_eligibility A, eligibility_all B
		where B.merch_id=A.Parent_merch and B.merch_level-10 = A.parent_merch_level;
	quit;

/*=STEP:011 END========================================================================================================================================*/


/*=STEP:012 START=============================Agreegate Over time====================================================================================*/
	data meligibility50;
	set eligibility_all (obs=0);
	run;
	data meligibility49;
	set eligibility_all (obs=0);
	run;
	data meligibility48;
	set eligibility_all (obs=0);
	run;
	data meligibility47;
	set eligibility_all (obs=0);
	run;


	proc sql;
		insert into meligibility50
			select a.merch_id,a.merch_level,a.location_id,a.location_level,b.lv4time_lkup_id as time_id,50 as time_level, max(eligibility_flag)
			from meligibility a, maxapp.lv5time b
			where a.time_id=b.lv5time_lkup_id
			group by a.merch_level,a.merch_id,a.location_level,a.location_id,b.lv4time_lkup_id ;


		insert into meligibility49
			select a.merch_id,a.merch_level,a.location_id,a.location_level,b.lv3time_lkup_id as time_id,49 as time_level, max(eligibility_flag)
			from meligibility a, maxapp.lv5time b
			where a.time_id=b.lv5time_lkup_id
			group by a.merch_level,a.merch_id,a.location_level,a.location_id,b.lv3time_lkup_id ;

		insert into meligibility48
			select a.merch_id,a.merch_level,a.location_id,a.location_level,b.lv2time_lkup_id as time_id,48 as time_level, max(eligibility_flag)
			from meligibility a, maxapp.lv5time b
			where a.time_id=b.lv5time_lkup_id
			group by a.merch_level,a.merch_id,a.location_level,a.location_id,b.lv2time_lkup_id ;


		insert into meligibility47
			select a.merch_id,a.merch_level,a.location_id,a.location_level,b.lv1time_lkup_id as time_id,47 as time_level, max(eligibility_flag)
			from meligibility a, maxapp.lv5time b
			where a.time_id=b.lv5time_lkup_id
			group by a.merch_level,a.merch_id,a.location_level,a.location_id,b.lv1time_lkup_id ;
	quit;


	data sasdata.meligibility;
	if 0 then set meligibility;
	length POA $80. like_store $80.;
	set meligibility meligibility50 meligibility49 meligibility48 meligibility47 end=last;
	if last then do;
	   call symputx('meligibility_cnt',_N_);
	end;
	run;
/*=STEP:012 END=====================================================================================================================================*/


/* ==STEP:012.5 == Get like store data and insert into LIKESTORE column ====== */

proc sql;
create table like_store_data as
select distinct location_id,plan_like_loc_id as lv4loc_id from maxdata.attr_ml
where plan_like_loc_id is not null;
quit;

proc sort data=like_store_data;
by lv4loc_id;
run;

proc sort data=maxdata.lv4loc out=lv4loc;
by lv4loc_id;
run;

data sasdata.lv4loc_likestore_data (drop=lv4loc_userid);
length like_store $80.;
merge lv4loc (in=in1 keep=lv4loc_id lv4loc_userid) like_store_data (in=in2 keep=location_id lv4loc_id);
by lv4loc_id;
like_store=lv4loc_userid;
run;


/*==STEP:013 START===== TRUNCATE TABLE MAXDATA.MELIGIBILITY =========================================================================================*/
	%macro truncate_check;
	%if %exist(maxdata.meligibility)=1
	  %then %do;
	    %logger(m,info,%bquote([truncate_check] Found table MAXDATA.meligibility PROCEEDING TO TRUNCATE BEFORE UPLOAD))
	    proc sql;
          %connect_to_db(maxdata);
		    execute ( truncate table maxdata.meligibility) by mmx_db;
		  disconnect from mmx_db;
		quit;
	  %end;
	%else %do;
	    %logger(m,warn,%bquote([truncate_check] Table MAXDATA.meligibility not found PROCEEDING TO create table))
	    proc sql;
	      create table maxdata.meligibility as
	      select * from sasdata.meligibility
	      where 1=0;
	    quit;
	%end;
	%mend truncate_check;
	%truncate_check  

/*==STEP:013 START=================================================================================================================================*/

/*
%logger(m,info, %bquote([:s7a mdin_bealls_eligibility] Uploading &meligibility_cnt records into maxdata.meligibility table))
*/
/*==STEP:014 START=====================Write back data to Oracle====================================================================================*/

/*
proc append base=MAXDATAW.meligibility (BULKLOAD =YES )
			data=sasdata.meligibility;
run;
*/
/*==STEP:014 END====================================================================================================================================*/


%timestamp(b,mdin_like_store_eligibility)
%include noncore(mdin_like_store_eligibility.sas);
%timestamp(e,mdin_like_store_eligibility)