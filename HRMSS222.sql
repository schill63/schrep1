--*-----------------------------------------------------------------------------
--*
--* FILE NAME..: HRMSS222.sql
--* AUTHOR.....: D. Wohlers   
--* DATE.......: 05/18/2010     
--*
--* DESCRIPTION:
--*        Update Medicare Exempt flag to N for Transitional Employees 
--*                                                                    
--*-----------------------------------------------------------------------------
--*
--*-----------------START_OF_MODIFICATIONS_LOG----------------------------------
--* MM/DD/YYYY  RefNum  Ename     Description Of Change-Newest to Oldest
--* ==========  ======  ========  ==============================================
--* XXXXXXXXXXXXXXXXXXXXXXXXXXXX test GIT
--* 06/26/2012  T08576  JMWILKIN  Use new header/modlog format; mods for stds
--*-----------------END_OF_MODIFICATIONS_LOG------------------------------------
set serveroutput on

--*  BEGIN PROCESS

declare

  file_handle             utl_file.file_type;

  outpath                 varchar2(255) := '&utl_path';
  outfile                 varchar2(255) := '&utl_file1';  -- Updates and errors 

--*--------------------------------------------------------------------*
--* Declare variables
--*--------------------------------------------------------------------*


  l_comment               varchar2(200)  := null;
  l_error_msg             varchar2(255);
  l_print_file            varchar2(1)    := 'N';
  l_report_line		  varchar2(2000) := null;
  l_subhead_line	  varchar2(2000) := null;
  l_line_cnt              number(5)      :=0;


CURSOR trans_emp is
   select asg.assignment_number
         ,asg.effective_start_date     asg_start_date
         ,peo.full_name
         ,ftr.medicare_tax_exempt
         ,ftr.emp_fed_tax_rule_id
         ,ftr.object_version_number
         ,ftr.effective_start_date    fed_tax_start_date
         ,ftr.effective_end_date
   from   per_all_people_f            peo
         ,pay_us_emp_fed_tax_rules_f  ftr
         ,per_all_assignments_f       asg
   where  asg.assignment_status_type_id in (81,82) -- Transitional Active/Inactive 
   and    sysdate between asg.effective_start_date and asg.effective_end_date
   and    ftr.assignment_id = asg.assignment_id
   and    sysdate between ftr.effective_start_date and ftr.effective_end_date
   and    ftr.medicare_tax_exempt = 'Y'
   and    peo.person_id = asg.person_id
   and    sysdate between peo.effective_start_date and peo.effective_end_date
   and    csuh_custom.is_active_asg(asg.assignment_id, sysdate) = 'Y' 
   order by peo.full_name;

--*--------------------------------------------------------------------*
--* Update medicare tax exempt flag value passed into function. 
--*--------------------------------------------------------------------*
Function update_medicare_flag (v_emp_fed_tax_rule_id   number,
                               v_object_version_number number,
                               v_date                  date)
         return varchar is

l_effective_start_date    date;
l_effective_end_date      date;
l_emp_fed_tax_rule_id     number(9);
l_object_version_number   number;
l_validate                boolean      := FALSE;
l_update_flag             varchar2(1)  := 'Y';

begin
   l_object_version_number := v_object_version_number;
   l_emp_fed_tax_rule_id   := v_emp_fed_tax_rule_id;
   begin
      apps.pay_federal_tax_rule_api.update_fed_tax_rule
            (p_validate                     => l_validate
            ,p_datetrack_update_mode        => 'UPDATE'
            ,p_effective_date               => v_date
            ,p_object_version_number        => l_object_version_number
            ,p_emp_fed_tax_rule_id          => l_emp_fed_tax_rule_id
            ,p_medicare_tax_exempt          => 'N'
            ,p_effective_start_date         => l_effective_start_date
            ,p_effective_end_date           => l_effective_end_date
            );
      exception
         when others then
            l_update_flag     := 'N';
   end;

   return (l_update_flag);

END update_medicare_flag;


--*--------------------------------------------------------------------*
--*--------------------------------------------------------------------*
--*--------------------------------------------------------------------*
--*--------------------------------------------------------------------*
BEGIN

  dbms_output.enable (1000000);

  dbms_output.put_line('**** Start of HRMSS222 ' ||to_char(sysdate,'MM/DD/YYYY HH24:MI:SS'));

  file_handle := utl_file.fopen (outpath,outfile,'W');
  
  l_subhead_line := (                                   '"' ||           
                     'Full Name'                     ||'","'||     
                     'Asg #'                         ||'","'||
                     'Original Fed Tax Start Date'   ||'","'||
                     'Asg Start Date'                ||'","'||
                     'Comments'                      ||'"') ;


  For X in trans_emp LOOP


     l_comment               := null;
     l_print_file            := 'N';

--*--------------------------------------------------------------------*
--* update medicare exempt flag
--*--------------------------------------------------------------------*
 
     if update_medicare_flag (x.emp_fed_tax_rule_id, 
                              x.object_version_number,
                              x.asg_start_date)              = 'N' then
        l_comment          := l_comment||'Can not update medicare tax flag; possible future '||
                                         'dated entry exists; update manually; ';
        l_print_file       := 'Y';
     else
        l_comment          := l_comment||'Medicare exempt flag updated to N; ';
        l_print_file       := 'Y';
     end if; 


--*--------------------------------------------------------------------*
--* Print output files.
--*--------------------------------------------------------------------*
     l_report_line := (                                  '"'|| 
                       x.full_name                   ||'","'||
                       x.assignment_number           ||'","'||
                       x.fed_tax_start_date          ||'","'||
                       x.asg_start_date              ||'","'||
                       l_comment                     ||'"');

     if l_print_file = 'Y' then
        if l_line_cnt = 0 then
           utl_file.put_line(file_handle, l_subhead_line);
        end if;
        utl_file.put_line(file_handle, l_report_line);
        l_line_cnt := l_line_cnt + 1;
     end if;                   

  End Loop;


  utl_file.fflush(file_handle);

  dbms_output.put_line ('+-----------------------------------------+');
  dbms_output.put_line ('|         HRMSS222  JOB SUMMARY           |');
  dbms_output.put_line ('+-----------------------------------------+');
  dbms_output.put_line ('Total records for outfile = '||l_line_cnt);
  dbms_output.put_line
        ('**** End   of HRMSS222 ' || to_char(sysdate,'MM/DD/YYYY HH24:MI:SS'));

Exception
  When no_data_found Then
         DBMS_OUTPUT.PUT_LINE('Failed: no_data_found') ;
         DBMS_OUTPUT.PUT_LINE(to_char(sqlcode) ||' '|| substr(sqlerrm,1,250)) ;
         raise_application_error(-20000,'Failed: '||l_error_msg);
  When too_many_rows Then
         DBMS_OUTPUT.PUT_LINE('Failed: too_many_rows') ;
         DBMS_OUTPUT.PUT_LINE(to_char(sqlcode) ||' '|| substr(sqlerrm,1,250)) ;
         raise_application_error(-20000,'Failed: '||l_error_msg);
  When Others then
         DBMS_OUTPUT.PUT_LINE('Failed: others') ;
         DBMS_OUTPUT.PUT_LINE(to_char(sqlcode) ||' '|| substr(sqlerrm,1,250)) ;
         raise_application_error(-20000,'Failed: '||to_char(sqlcode)||' '||substr(sqlerrm,1,250));

end;
/
--*--------------------------------------------------------------------*
--*  END THE PL/SQL BLOCK
--*--------------------------------------------------------------------*

--*--------------------------------------------------------------------*
--*                                                                    *
--* HRMSS222.sql                                                       *
--*                                                                    *
--*--------------------------------------------------------------------*
