--*-----------------------------------------------------------------------------
--*
--* FILE NAME..: HRMSS175.sql
--* AUTHOR.....: Bob Van...        
--* DATE.......: 4/27/2009 
--*
--* DESCRIPTION:
--*        Gather KFS Earnings and deduction Transactions              
--*                                                                    
--*-----------------------------------------------------------------------------
--*
--*-----------------START_OF_MODIFICATIONS_LOG----------------------------------
--* MM/DD/YYYY  RefNum  Ename     Description Of Change-Newest to Oldest
--* ==========  ======  ========  ==============================================
--* 01/22/2016  1286777 SCHILL    Identify chart code from csuf_accounts table
--* 06/19/2015  812231  SCHILL    Use csu_utility.character_to_date function
--*                               instead of to_date
--* 08/20/2014  432807  SCHILL    Include sub accounts when sending data to KFS
--*                               for deductions. Currently only being sent for earnings.
--* 06/17/2013  I04941  SCHILL    Modify research leave to handle negative entries
--* 04/11/2013  I04698  SCHILL    Add l_run_type to Research Leave query
--* 02/07/2013  I04698  SCHILL    Exclude Research Leave when associated with Quickpays
--* 11/02/2012  I04603  SCHILL    Fix Research Leave for quickpays. Modify get_emp cursor - DW
--* 09/12/2012  I04498  SCHILL    Get fringe from link if it exists on link not
--*                               just for FGT
--* 07/19/2012  T07911  bnewton   Research leave and FGT changes
--* 06/26/2012  T08576  JMWILKIN  Use new header/modlog format; mods for stds
--*-----------------END_OF_MODIFICATIONS_LOG------------------------------------

--*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--*                     START_OF_PREVIOUS_FORMAT_MODLOG_SECTION
--*        DO NOT ENTER NEW MODLOG ENTRIES IN PREVIOUS_FORMAT_MODLOG_SECTION
--*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--*--------------------------------------------------------------------*
--*                                                                    *
--*  NAME: HRMSS175.sql      AUTHOR: Bob Van...        DATE: 4/27/2009 *
--* TITLE: Gather KFS Earnings and deduction Transactions              *
--*                                                                    *
--* DATE       INIT SSR # REASON FOR THE CHANGE                        *
--* ---------- ---- ----- -------------------------------------------- *
--* 06/16/2004  KM        Added csuh_fap_assess_rate                   *
--* 07/07/2004  KM        Added logic for Perf Pay Norecover           *
--* 11/22/2004  KM        Added +RULE to select statements             *
--* 07/20/2005  KM        Changed batch date to sysdate                *
--* 06/18/2007  KM  2862  select from csuh_coa_v using l_fiscal_year   *
--*                                                                    *
--*--------------------------------------------------------------------*
--* MM/DD/CCYY-INIT-CLARITY#--DESCRIPTION OF CHANGE                    *
--* ------------------------------------------------------------------ *
--* 03/25/2008  KG  I00448  Added code to section 'get non ER liability*
--*                         deductions' to handle EIC ADJUSTMENTS      *
--* 11/24/2008--SS--T00443--Convert for Appworx                        *
--* 04/27/2009  BV  T02924  Re-write from HRMSS056.sql for KFS         *
--* 07/13/2009  KLM T02924 Use sysdate for TransDate, Put PPE date in  *
--*                        Org Doc Id.                                 *
--* 07/28/2009  KLM T03237 Negative amounts (LWOP) post as credit      *
--* 07/31/2009  KLM T03234 Transaction for Bank 05                     *
--* 08/06/2009--KM--T03254--Send subacct on certain fringe transactions*
--* 08/97/2009  KLM T03237 Negative Deduction post as debit            *
--* 08/31/2009  KLM T03234 Use obj 1103 for Bank 05 transaction        *
--* 09/05/2009  KLM T03234 Change Bank 05 transaction to credit        *
--* 10/14/2009  KLM T03234 Removed Third Party Checks transacitons     *
--* 10/15/2009  KLM T03234 Add Third Party Check transactions AND      *
--*                        associated Bank 05 transactions.            *
--* 04/20/2010  BV  T04883 If July and fiscal year is same as current  *
--*                        year, fill in fiscal period with 13.        *
--* 07/18/2012  DW         Add FGT processing; Add Research Leave      *
--* mm/dd/ccyy--xx--Tnnnnn--Description-last line of modlog-don't delete
--*                                                                    *
--*--------------------------------------------------------------------*
--* NOTE: Always keep the mm/dd/yyyy template entry in above modlog    *
--*       Insert latest modlog entry above mm/dd/yy template entry.    *
--*       For TRAC#:                                                   *
--*       use Tnnnn for Tracker Task # or Innnn for Tracker Issue #    *
--*       For CLARITY#:                                                *
--*       use Tnnnnn for Clarity Task# or Innnnn for Clarity Incident# *
--*--------------------------------------------------------------------*
--*                                                                    *
--*--------------------------------------------------------------------*
--* Parameters:                                                        *
--*      &utl_path (directory for output file)                         *
--*      &utl_file1 (filename of output file [KFS .data file])         *
--*      &utl_file2 (filename of output file [KFS .recon file])        *
--*      &start_date date for which to extract earnings trans.         *
--*      &end_date  date for which to extract earnings trans.          *
--*      &payroll type  (21 for salary/22 for hourly)                  *
--*      &runtype       (Q for quickpay/R for regular)                 *
--*      &fiscal_year for batch header                                 *
--*--------------------------------------------------------------------*

--*  BEGIN PROCESS TO CREATE KFS TRANSACTIONS
set linesize 135

declare

 out_file1      utl_file.file_type;
 out_file2      utl_file.file_type;
 out_dir        varchar2(255) := '&utl_path';                              
 out_lis1       varchar2(255) := '&utl_file1';
 out_lis2       varchar2(255) := '&utl_file2';

 l_start_date   date   := trunc(to_date('&start_date','DD-MON-YYYY'));
 l_end_date     date   := trunc(to_date('&end_date','DD-MON-YYYY'));

 l_payroll      number := to_number('&payroll');    -- 21 salary, 22 hourly
 l_run_type     varchar2(1) := '&run_type';         -- Q quickpay, R Payroll Run
 l_fiscal_year  varchar2(4)  := '&fiscal_year';
 l_payroll_name varchar2(255);
 l_origin_code  varchar2(2);
 v_amt number;

 v_payroll_start_date        date;
 v_payroll_end_date          date;

 l_research_seq number;
 l_RL_count     number := 0;
 l_todays_date  date;
 
--****************************************
--* Get Earnings
--****************************************

 cursor get_pre_gen_amounts (l_start_date       date,
                             l_end_date         date) is
      Select /*+ RULE */ ppf.last_name||','||ppf.first_name||' '||ppf.middle_names empl_name
            ,substr(pcak.segment1,1,7)   asg_acct
            ,nvl(substr(pcak.segment2,1,4),'    ')   object
            ,substr(pcak.segment3,1,7)   fr_acct
            ,nvl(pcak.segment4,0)        fr_rate
            ,nvl(substr(pcak.segment5,1,4),'    ')   fr_object
            ,nvl(pcak.segment6,0)        partial_fr_rate
            ,nvl(substr(pcak.segment7,1,4),'    ')   partial_fr_object
            ,substr(pcak.segment8,1,4)   partial_fr_earnings_object
            ,nvl(cdi.CPP_RATE,nvl(pcak.segment10,0)) cpp_rate
            ,nvl(substr(pcak.segment9,1,4),'    ')   cpp_object
            ,nvl(substr(pcak.segment11,1,4),'    ')  cpp_fr_object
            ,DECODE(cdi.ADJUSTMENT,'Y','REDISTR',asg.assignment_number)  pid
            ,substr(pj.name,1,6)         job_class
            ,asg.assignment_id           assignment_id
            ,asg.payroll_id              payroll_id
            ,pet.element_name            element_name
            ,pet.element_type_id         element_type_id
            ,asg.people_group_id         people_group_id
            ,asg.ass_attribute7          flag_9over12
            ,gcc.segment1                segment1
            ,gcc.segment2                segment2
            ,dlh.distribution_date       effective_date
            ,cdi.PARTIAL_FRINGE          PARTIAL_FRINGE
            ,cdi.WORKSTUDY_PLAN          WORKSTUDY_PLAN
            ,cdi.EARNINGS_TYPE           EARNINGS_TYPE
            ,dlh.PRE_GEN_DIST_line_id    line_id
            ,'PRE_GEN'                   rec_type
           ,sum(Decode(dlh.DR_CR_FLAG,'D',1,-1) * dlh.DISTRIBUTION_AMOUNT) summary_amount
         from
             per_all_people_f                ppf
            ,pay_element_types_f             pet
            ,per_jobs                        pj
            ,pay_cost_allocation_keyflex     pcak
            ,gl_code_combinations            gcc
            ,per_all_assignments_f           asg
            ,csuh_distribution_interface     cdi
            ,psp_pre_gen_dist_lines_history  dlh
         where dlh.distribution_date between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id    = dlh.element_type_id
           and cdi.effective_date between ppf.effective_start_date and ppf.effective_end_date
           and ppf.person_id         = asg.person_id
           and pcak.cost_allocation_keyflex_id = cdi.cost_allocation_keyflex_id
           and dlh.distribution_date between nvl(pj.date_from,dlh.distribution_date) and nvl(pj.date_to,dlh.distribution_date)
           and pj.job_id            = asg.job_id
           and dlh.distribution_date between asg.effective_start_date and asg.effective_end_date
           and asg.assignment_id       = dlh.assignment_id
           and gcc.code_combination_id = dlh.gl_code_combination_id
           and cdi.ADJUSTMENT          = 'N'
           and cdi.DISTRIBUTION_INTERFACE_ID   = dlh.DISTRIBUTION_INTERFACE_ID
           and dlh.REVERSAL_ENTRY_FLAG is null
           and (cdi.PAYROLL_RUN_TYPE = l_run_type
               or (cdi.PAYROLL_RUN_TYPE = 'V' and l_run_type = 'Q')
               )
           and cdi.effective_date between l_start_date and l_end_date
  group by  ppf.last_name||','||ppf.first_name||' '||ppf.middle_names
            ,substr(pcak.segment1,1,7)
            ,substr(pcak.segment2,1,4)
            ,substr(pcak.segment3,1,7)
            ,nvl(pcak.segment4,0)
            ,substr(pcak.segment5,1,4)
            ,nvl(pcak.segment6,0)
            ,substr(pcak.segment7,1,4)
            ,substr(pcak.segment8,1,4)
            ,substr(pcak.segment9,1,4)
            ,nvl(cdi.CPP_RATE,nvl(pcak.segment10,0))
            ,substr(pcak.segment11,1,4)
            ,asg.assignment_number
            ,substr(pj.name,1,6)
            ,asg.payroll_id
            ,asg.assignment_id 
            ,pet.element_name
            ,pet.element_type_id
            ,asg.people_group_id
            ,asg.ass_attribute7
            ,gcc.segment1
            ,gcc.segment2
            ,dlh.distribution_date
            ,cdi.PARTIAL_FRINGE
            ,cdi.ADJUSTMENT
            ,cdi.WORKSTUDY_PLAN
            ,cdi.EARNINGS_TYPE
            ,dlh.PRE_GEN_DIST_line_id
UNION ALL
select /*+ RULE */ ppf.last_name||','||ppf.first_name||' '||ppf.middle_names empl_name
            ,substr(pcak.segment1,1,7)   asg_acct
            ,nvl(substr(pcak.segment2,1,4),'    ')   object
            ,substr(pcak.segment3,1,7)   fr_acct
            ,nvl(pcak.segment4,0)        fr_rate
            ,nvl(substr(pcak.segment5,1,4),'    ')   fr_object
            ,nvl(pcak.segment6,0)        partial_fr_rate
            ,nvl(substr(pcak.segment7,1,4),'    ')   partial_fr_object
            ,substr(pcak.segment8,1,4)   partial_fr_earnings_object
            ,to_number(nvl(pcak.segment10,0))        cpp_rate
            ,nvl(substr(pcak.segment9,1,4),'    ')   cpp_object
            ,nvl(substr(pcak.segment11,1,4),'    ')  cpp_fr_object
            ,pas.assignment_number       pid
            ,substr(pj.name,1,6)         job_class
            ,pas.assignment_id           assignment_id
            ,pas.payroll_id              payroll_id
            ,pet.element_name            element_name
            ,pet.element_type_id         element_type_id
            ,pas.people_group_id         people_group_id
            ,pas.ass_attribute7          flag_9over12
            ,''                          segment1
            ,''                          segment2
            ,ppa.effective_date          effective_date
            ,'N'                         PARTIAL_FRINGE
            ,''                          WORKSTUDY_PLAN
            ,csuh_earnings_type(pas.assignment_id, ppa.effective_date, pet.element_type_id)
                                         EARNINGS_TYPE
            ,pc.cost_id                  line_id
            ,'NONPAYROLL'                rec_type
            ,decode(pc.DEBIT_OR_CREDIT,'C',-1,1)*pc.costed_value  summary_amount
         from pay_element_classifications pec
             ,pay_cost_allocation_keyflex pcak
             ,per_all_people_f        ppf
             ,per_jobs                pj
             ,per_all_assignments_f   pas
             ,pay_element_types_f     pet
             ,pay_run_results         prr
             ,pay_costs               pc
             ,pay_assignment_actions  paa
             ,pay_payroll_actions     ppa
         where ppa.effective_date between ppf.effective_start_date and ppf.effective_end_date
           and ppf.person_id = pas.person_id
           and ppa.effective_date between nvl(pj.date_from,ppa.effective_date) and nvl(pj.date_to,ppa.effective_date)
           and pj.job_id            = pas.job_id
           and pcak.cost_allocation_keyflex_id = pc.cost_allocation_keyflex_id
           and ppa.effective_date between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = paa.assignment_id
           and pec.classification_id    = pet.classification_id
           and pet.element_name not like 'Adjust%'
           and ppa.effective_date between pet.effective_start_date and pet.effective_end_date
           and pet.classification_id||'' = 35
           and pet.element_type_id       = prr.element_type_id
           and prr.run_result_id         = pc.run_result_id
           and pc.balance_or_cost       = 'C'     -- debits only
           and pc.assignment_action_id  = paa.assignment_action_id
           and (CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = l_run_type
               or (CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = '?'
                   and l_run_type = 'Q')
               or (CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = 'V'
                   and l_run_type = 'Q')
               )
           and paa.payroll_action_id    = ppa.payroll_action_id
           and ppa.action_status = 'C'     -- complete
           and ppa.effective_date between l_start_date and l_end_date
           and ppa.action_type   = 'C';     -- costing


 cursor get_cash_payment_lines is
    select /*+ RULE */ substr(pcak.segment1,1,7) asg_acct
          ,substr(pcak.segment2,1,4) object
          ,pet.element_name
          ,ppf.last_name||','||ppf.first_name||' '||ppf.middle_names emp_name
          ,pas.assignment_number
          ,pas.assignment_id
          ,pet.element_type_id
          ,substr(pj.name,1,6)         job_class
          ,sum(decode(pc.DEBIT_OR_CREDIT,'D',-1,1)*pc.costed_value) costed_value
         from pay_element_classifications pec
             ,pay_cost_allocation_keyflex pcak
             ,per_jobs                    pj
             ,pay_element_types_f   pet
             ,per_all_people_f       ppf
             ,per_all_assignments_f  pas
             ,pay_run_results   prr
             ,pay_costs    pc
             ,pay_assignment_actions  paa
             ,pay_payroll_actions    ppa
         where pec.classification_id    = pet.classification_id
           and l_end_date between nvl(pj.date_from,l_end_date) and nvl(pj.date_to,l_end_date)
           and pj.job_id                = pas.job_id
           and ppa.effective_date between ppf.effective_start_date and ppf.effective_end_date
           and ppf.person_id = pas.person_id
           and pet.classification_id in (28      -- Involuntary Deductions
                                        ,30      -- Pre-Tax Deductions
                                        ,31      -- Tax Deductions
                                        ,32      -- Voluntary Deductions
                                        ,36      -- Tax Credits
                                        )
           and ppa.effective_date between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = prr.element_type_id
           and pcak.cost_allocation_keyflex_id = pc.cost_allocation_keyflex_id
           and ppa.effective_date between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = paa.assignment_id
           and EXISTS
                 (select 1
                     from pay_payroll_Actions    ppa1
                         ,pay_run_results        prr1
                         ,pay_assignment_actions paa1
                         ,pay_action_interlocks  pai
                    where ppa1.action_type = 'B'
                      and ppa1.payroll_Action_id = paa1.payroll_Action_id
                      and prr1.element_type_id||''   = prr.element_type_id
                      and prr1.ASSIGNMENT_ACTION_ID = paa1.ASSIGNMENT_ACTION_ID
                      and paa1.assignment_action_id = LOCKED_ACTION_ID
                      and pai.LOCKING_ACTION_ID = paa.assignment_action_id
                 )
           and prr.run_result_id         = pc.run_result_id
           and pc.balance_or_cost       = 'C'     -- debits only
           and pc.assignment_action_id  = paa.assignment_action_id
           and paa.payroll_action_id    = ppa.payroll_action_id
           and ppa.action_status = 'C'     -- complete
           and ppa.action_type   = 'C'     -- Costing
           and ppa.effective_date between l_start_date and l_end_date
           and ppa.payroll_id = l_payroll
  group by pet.element_name
          ,ppf.last_name||','||ppf.first_name||' '||ppf.middle_names
          ,pas.assignment_number
          ,pas.assignment_id
          ,pet.element_type_id
          ,substr(pj.name,1,6)
          ,substr(pcak.segment1,1,7)
          ,substr(pcak.segment2,1,4);


-- *******************************
-- * get assignnments for net
-- *******************************
 cursor net_records  (l_beg_date DATE, l_end_date DATE, l_payroll NUMBER) is
    select /*+ RULE */ paa.assignment_action_id   Run_no
        from per_all_assignments_f   asg,
             pay_assignment_actions  paa,
             pay_payroll_actions     ppa
       where asg.payroll_id = l_payroll
         and ppa.date_earned between asg.effective_start_date and asg.effective_end_date
         and asg.assignment_id       = paa.assignment_id
         and paa.payroll_action_id   = ppa.payroll_action_id
         and (ppa.ACTION_TYPE        = l_run_type
              or (ppa.ACTION_TYPE = '?' and l_run_type='Q')
              or (ppa.ACTION_TYPE = 'V' and l_run_type='Q')
             )
         and ppa.effective_date between l_beg_date and l_end_date ;


--*********************************************
--* Deductions - non ER liabilities
--*********************************************
 cursor get_deductions_summary_lines is
    select /*+ RULE */ substr(pcak.segment1,1,7) asg_acct                  -- Deductions
          ,substr(pcak.segment13,1,5) asg_sub_acct  -- schill - 432807
          ,substr(pcak.segment2,1,4) object
          ,pet.element_name
          ,pet.attribute6            ded_contr_code
          ,sum(decode(pc.DEBIT_OR_CREDIT,'D',-1,1)*pc.costed_value) costed_value
          ,classification_name
          ,'Reg' line_type
         from pay_element_classifications pec
             ,pay_cost_allocation_keyflex pcak
             ,pay_element_types_f   pet
             ,per_all_assignments_f  pas
             ,pay_run_results   prr
             ,pay_costs    pc
             ,pay_assignment_actions  paa
             ,pay_payroll_actions    ppa
         where pec.classification_id    = pet.classification_id
           and pcak.cost_allocation_keyflex_id = pc.cost_allocation_keyflex_id
           and ppa.effective_date between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = paa.assignment_id
           and (pet.classification_id in (28      -- Involuntary Deductions
                                        ,30      -- Pre-Tax Deductions
                                        ,31      -- Tax Deductions
                                        ,32      -- Voluntary Deductions
                                        ,36      -- Tax Credits
                                        )
                 or pet.classification_id = 35 and pet.element_name like 'Adjust%')
           and ppa.effective_date between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = prr.element_type_id
           and NOT EXISTS
                 (select 1
                     from pay_payroll_Actions    ppa1
                         ,pay_run_results        prr1
                         ,pay_assignment_actions paa1
                         ,pay_action_interlocks  pai
                    where ppa1.action_type = 'B'
                      and ppa1.payroll_Action_id = paa1.payroll_Action_id
                      and prr1.element_type_id||''   = prr.element_type_id
                      and prr1.ASSIGNMENT_ACTION_ID = paa1.ASSIGNMENT_ACTION_ID
                      and paa1.assignment_action_id = LOCKED_ACTION_ID
                      and pai.LOCKING_ACTION_ID = paa.assignment_action_id
                 )
           and prr.run_result_id         = pc.run_result_id
           and pc.balance_or_cost       = 'C'     -- debits only
           and pc.assignment_action_id  = paa.assignment_action_id
           and (CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = l_run_type
               or (CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = '?'
                   and l_run_type = 'Q')
               or (CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = 'V'
                   and l_run_type = 'Q')
               )
           and paa.payroll_action_id    = ppa.payroll_action_id
           and ppa.action_status = 'C'     -- complete
           and ppa.action_type   = 'C'     -- costing
           and ppa.effective_date between l_start_date and l_end_date
           and ppa.payroll_id = l_payroll
  group by pet.element_name
     ,classification_name
     ,pet.attribute6
     ,substr(pcak.segment1,1,7)
     ,substr(pcak.segment13,1,5)  -- schill - 432807
     ,substr(pcak.segment2,1,4)
     ,'Reg'
UNION ALL
    select /*+ RULE */ substr(pcak.segment1,1,7) asg_acct
          ,substr(pcak.segment13,1,5) asg_sub_acct  -- schill - 432807
          ,substr(pcak.segment2,1,4) object
          ,pet.element_name
          ,pet.attribute6            ded_contr_code
          ,sum(decode(pc.DEBIT_OR_CREDIT,'D',-1,1)*pc.costed_value) costed_value
          ,classification_name
          ,'Reg' line_type
         from pay_element_classifications pec
             ,pay_cost_allocation_keyflex pcak
             ,pay_element_types_f   pet
             ,per_all_assignments_f  pas
             ,pay_run_results   prr
             ,pay_costs    pc
             ,pay_assignment_actions  paa
             ,pay_payroll_actions    ppa
         where pec.classification_id    = pet.classification_id
           and pcak.cost_allocation_keyflex_id = pc.cost_allocation_keyflex_id
           and ppa.effective_date between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = paa.assignment_id
           and pet.classification_id in (28      -- Involuntary Deductions
                                        ,30      -- Pre-Tax Deductions
                                        ,31      -- Tax Deductions
                                        ,32      -- Voluntary Deductions
                                        ,36      -- Tax Credits
                                        )
           and ppa.effective_date between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = prr.element_type_id
           and EXISTS
                 (select 1
                     from pay_payroll_Actions    ppa1
                         ,pay_run_results        prr1
                         ,pay_assignment_actions paa1
                         ,pay_action_interlocks  pai
                    where ppa1.action_type = 'B'
                      and ppa1.payroll_Action_id = paa1.payroll_Action_id
                      and prr1.element_type_id||''   = prr.element_type_id
                      and prr1.ASSIGNMENT_ACTION_ID = paa1.ASSIGNMENT_ACTION_ID
                      and paa1.assignment_action_id = LOCKED_ACTION_ID
                      and pai.LOCKING_ACTION_ID = paa.assignment_action_id
                 )
           and prr.run_result_id        = pc.run_result_id
           and pc.balance_or_cost       = 'C'     -- debits only
           and pc.assignment_action_id  = paa.assignment_action_id
           and paa.payroll_action_id    = ppa.payroll_action_id
           and ppa.action_status = 'C'   -- complete
           and ppa.action_type = 'C'     -- Costing
           and ppa.effective_date between l_start_date and l_end_date
           and ppa.payroll_id = l_payroll
           and l_run_type = 'R'
  group by pet.element_name
     ,classification_name
     ,pet.attribute6
     ,substr(pcak.segment1,1,7)
     ,substr(pcak.segment13,1,5)  -- schill - 432807
     ,substr(pcak.segment2,1,4)
     ,'Reg'
UNION ALL                                                         --  Tax Arrears
    select /*+ RULE */ substr(pcak.segment1,1,7) asg_acct
          ,substr(pcak.segment13,1,5) asg_sub_acct  -- schill - 432807
          ,substr(pcak.segment2,1,4) object
          ,pet.element_name
          ,pet.attribute6            ded_contr_code
          ,sum(decode(pc.DEBIT_OR_CREDIT,'D',-1,1)*pc.costed_value) costed_value
          ,classification_name
          ,'Reg' line_type
         from pay_element_classifications pec
             ,pay_cost_allocation_keyflex pcak
             ,pay_element_sets       pes
             ,pay_element_type_rules petr
             ,pay_element_types_f   pet
             ,per_all_assignments_f  pas
             ,pay_run_results   prr
             ,pay_costs    pc
             ,pay_assignment_actions  paa
             ,pay_payroll_actions    ppa
         where pes.ELEMENT_SET_NAME = 'CSU_TAX_ARREARS'
           AND pes.element_set_id = petr.element_set_id
           AND petr.INCLUDE_OR_EXCLUDE = 'I'
           AND petr.element_type_id = pet.element_type_id
           and pet.classification_id    = pec.classification_id
           and ppa.effective_date between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = prr.element_type_id
           and pcak.cost_allocation_keyflex_id = pc.cost_allocation_keyflex_id
           and ppa.effective_date between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = paa.assignment_id
           and prr.run_result_id         = pc.run_result_id
           and pc.balance_or_cost       = 'C'     -- debits only
           and pc.assignment_action_id  = paa.assignment_action_id
           and ((ppa.action_type = 'C' and CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = l_run_type)
             or (ppa.action_type = 'B' and l_run_type = 'R')
             or (ppa.action_type = 'C' and CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = '?'
                 and l_run_type  = 'Q')
             or (ppa.action_type = 'C' and CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = 'V'
                 and l_run_type  = 'Q')
               )
           and paa.payroll_action_id    = ppa.payroll_action_id
           and ppa.action_status = 'C'     -- complete
           and ppa.action_type IN ('C','B')  -- costing
           and ppa.effective_date between l_start_date and l_end_date
           and ppa.payroll_id = l_payroll
  group by pet.element_name
     ,classification_name
     ,pet.attribute6
     ,substr(pcak.segment1,1,7)
     ,substr(pcak.segment13,1,5) -- schill - 432807
     ,substr(pcak.segment2,1,4)
     ,'Reg'
UNION ALL                                       -- Take Out Double Dedcutions in Apr, May
    select /*+ RULE */ substr(pcak.segment1,1,7) asg_acct
          ,substr(pcak.segment13,1,5) asg_sub_acct  -- schill - 432807
          ,substr(pcak.segment2,1,4) object
          ,pet.element_name
          ,pet.attribute6            ded_contr_code
          ,sum(nvl(dd.ELEMENT_ENTRY_VALUE,0) * -1) costed_value
          ,classification_name
          ,'DD' line_type
         from pay_element_classifications pec
             ,pay_cost_allocation_keyflex pcak
             ,pay_element_entries_f  pee
             ,pay_element_links_f    pel
             ,pay_element_types_f    pet
             ,per_all_assignments_f  pas
             ,csuh_double_deductions dd
         where pec.classification_id    = pet.classification_id
           and pcak.cost_allocation_keyflex_id = pel.cost_allocation_keyflex_id
           and dd.ELEMENT_EFFECTIVE_DATE between pee.effective_start_date and pee.effective_end_date
           and pee.element_link_id      = pel.element_link_id
           and pee.assignment_id        = pas.assignment_id
           and dd.ELEMENT_EFFECTIVE_DATE between pel.effective_start_date and pel.effective_end_date
           and pel.element_type_id      = pet.element_type_id
           and pet.classification_id in (28      -- Involuntary Deductions
                                        ,30      -- Pre-Tax Deductions
                                        ,31      -- Tax Deductions
                                        ,32      -- Voluntary Deductions
                                        ,36      -- Tax Credits
                                        )
           and dd.ELEMENT_EFFECTIVE_DATE between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = dd.BASE_ELEMENT_ID
           and pas.payroll_id||''       = l_payroll
           and dd.ELEMENT_EFFECTIVE_DATE between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = dd.assignment_id
           and to_char(dd.ELEMENT_EFFECTIVE_DATE,'mmyyyy') = to_char(l_end_date, 'mmyyyy')
           and l_run_type = 'R'
  group by pet.element_name
     ,classification_name
     ,pet.attribute6
     ,substr(pcak.segment1,1,7)
     ,substr(pcak.segment13,1,5) -- schill - 432807
     ,substr(pcak.segment2,1,4)
     ,'DD'
UNION ALL                                       -- Put in Double Dedcutions in jun, jul
    select /*+ RULE */ substr(pcak.segment1,1,7) asg_acct
          ,substr(pcak.segment13,1,5) asg_sub_acct  -- schill - 432807
          ,substr(pcak.segment2,1,4) object
          ,pet.element_name
          ,pet.attribute6            ded_contr_code
          ,sum(nvl(dd.ELEMENT_ENTRY_VALUE,0)) costed_value
          ,classification_name
          ,'DD'  line_type
         from pay_element_classifications pec
             ,pay_cost_allocation_keyflex pcak
             ,pay_element_entries_f  pee
             ,pay_element_links_f    pel
             ,pay_element_types_f    pet
             ,per_all_assignments_f  pas
             ,csuh_double_deductions dd
         where pcak.cost_allocation_keyflex_id = pel.cost_allocation_keyflex_id
           and pec.classification_id    = pet.classification_id
           and dd.ELEMENT_EFFECTIVE_DATE between pee.effective_start_date and pee.effective_end_date
           and pee.element_link_id      = pel.element_link_id
           and pee.assignment_id        = pas.assignment_id
           and dd.ELEMENT_EFFECTIVE_DATE between pel.effective_start_date and pel.effective_end_date
           and pel.element_type_id      = pet.element_type_id
           and pet.classification_id in (28      -- Involuntary Deductions
                                        ,30      -- Pre-Tax Deductions
                                        ,31      -- Tax Deductions
                                        ,32      -- Voluntary Deductions
                                        ,36      -- Tax Credits
                                        )
           and dd.ELEMENT_EFFECTIVE_DATE between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = dd.BASE_ELEMENT_ID
           and pas.payroll_id||''       = l_payroll
           and dd.ELEMENT_EFFECTIVE_DATE between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = dd.assignment_id
           and to_char(dd.BATCH_PROCESS_DATE,'mmyyyy') = to_char(l_end_date, 'mmyyyy')
           and l_run_type = 'R'
  group by pet.element_name
     ,classification_name
     ,pet.attribute6
     ,substr(pcak.segment1,1,7)
     ,substr(pcak.segment13,1,5)  -- schill - 432807
     ,substr(pcak.segment2,1,4)
     ,'DD';


--*********************************************
--* Deductions - ER liabilities
--*********************************************
 cursor get_ded_er_summary_lines is
    select /*+ RULE */ pet.element_name
          ,pet.attribute6            ded_contr_code
          ,substr(cost.segment1,1,7) db_acct
          ,substr(cost.segment13,1,5) db_sub_acct  -- schill - 432807
          ,substr(cost.segment2,1,4) db_object
          ,substr(bal.segment1,1,7)  cr_acct
          ,substr(bal.segment13,1,5)  cr_sub_acct  -- schill - 432807
          ,substr(bal.segment2,1,4)  cr_object
          ,sum(decode(pc.DEBIT_OR_CREDIT,'C',-1,1)*pc.costed_value) costed_value
      from pay_cost_allocation_keyflex cost
             ,pay_cost_allocation_keyflex bal
             ,pay_element_classifications pec
             ,pay_element_links_f    pel
             ,per_all_assignments_f  pas
             ,pay_element_types_f   pet
             ,pay_run_results   prr
             ,pay_costs    pc
             ,pay_assignment_actions  paa
             ,pay_payroll_actions   ppa
         where bal.COST_ALLOCATION_KEYFLEX_ID = pel.BALANCING_KEYFLEX_ID
           and cost.cost_allocation_keyflex_id = pel.cost_allocation_keyflex_id
           and ppa.effective_date between pel.effective_start_date and pel.effective_end_date
       and ( pas.job_id = nvl(pel.job_id,pas.job_id) and
          nvl(pas.position_id,0) = nvl(nvl(pel.position_id,pas.position_id),0) and
          pas.people_group_id = nvl(pel.people_group_id,pas.people_group_id) and
          pas.organization_id = nvl(pel.organization_id,pas.organization_id) and
          pas.location_id = nvl(pel.location_id,pas.location_id) and
          nvl(pas.grade_id,0) = nvl(nvl(pel.grade_id,pas.grade_id),0) and
          pas.pay_basis_id = nvl(pel.pay_basis_id,pas.pay_basis_id) and
          nvl(pas.employment_category,0) = nvl(nvl(pel.employment_category,pas.employment_category),0) and
          (pas.payroll_id = nvl(pel.payroll_id,pas.payroll_id) or pel.link_to_all_payrolls_flag = 'Y')
        )
           and pel.element_type_id      = pet.element_type_id
           and ppa.effective_date between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = paa.assignment_id
           and pec.classification_id    = pet.classification_id
           and pet.classification_id    in (29    -- Employer Taxes
                                           ,33    -- Employer Liabilities
                                           )
           and ppa.effective_date between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = prr.element_type_id
           and prr.run_result_id        = pc.run_result_id
           and pc.balance_or_cost = 'C'     -- credits only
           and pc.assignment_action_id  = paa.assignment_action_id
           and (CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = l_run_type
             or (CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = '?' and l_run_type  = 'Q')
             or (CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = 'V' and l_run_type  = 'Q')
               )
           and paa.payroll_action_id    = ppa.payroll_action_id
           and ppa.action_status  = 'C'     -- complete
           and ppa.action_type    = 'C'     -- costing
           and ppa.effective_date between l_start_date and l_end_date
           and ppa.payroll_id    = l_payroll
  group by pet.element_name
          ,pet.attribute6
          ,substr(cost.segment1,1,7)
          ,substr(cost.segment13,1,5)  -- schill - 432807
          ,substr(cost.segment2,1,4)
          ,substr(bal.segment1,1,7)
          ,substr(bal.segment13,1,5)    -- schill - 432807
          ,substr(bal.segment2,1,4)
UNION ALL                                       -- Take Out Double Dedcutions in Apr, May
    select /*+ RULE */ pet.element_name
          ,pet.attribute6            ded_contr_code
          ,substr(pcak.segment1,1,7) db_acct
          ,substr(pcak.segment13,1,5) db_sub_acct  -- schill - 432807
          ,'2201'                    db_object
          ,'2462010'                 cr_acct
          ,null                      cr_sub_acct  -- schill - 432807
          ,'1627'                    cr_object
          ,sum(nvl(dd.ELEMENT_ENTRY_VALUE,0) * -1) costed_value
         from pay_element_classifications pec
             ,pay_cost_allocation_keyflex pcak
             ,pay_element_entries_f  pee
             ,pay_element_links_f    pel
             ,pay_element_types_f    pet
             ,per_all_assignments_f  pas
             ,csuh_double_deductions dd
         where pcak.cost_allocation_keyflex_id = pel.cost_allocation_keyflex_id
           and pec.classification_id    = pet.classification_id
           and dd.ELEMENT_EFFECTIVE_DATE between pee.effective_start_date and pee.effective_end_date
           and pee.element_link_id      = pel.element_link_id
           and pee.assignment_id        = pas.assignment_id
           and dd.ELEMENT_EFFECTIVE_DATE between pel.effective_start_date and pel.effective_end_date
           and pel.element_type_id      = pet.element_type_id
           and pet.classification_id    in (29    -- Employer Taxes
                                           ,33    -- Employer Liabilities
                                           )
           and dd.ELEMENT_EFFECTIVE_DATE between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = dd.BASE_ELEMENT_ID
           and pas.payroll_id||''       = l_payroll
           and dd.ELEMENT_EFFECTIVE_DATE between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = dd.assignment_id
           and to_char(dd.ELEMENT_EFFECTIVE_DATE,'mmyyyy') = to_char(l_end_date, 'mmyyyy')
           and l_run_type = 'R'
  group by pet.element_name
          ,pet.attribute6
          ,substr(pcak.segment1,1,7)
          ,substr(pcak.segment13,1,5)  -- schill - 432807
          ,'2201'
          ,'2462010'
          ,'1627'
UNION ALL                                       -- Put in Double Dedcutions in jun, jul
    select /*+ RULE */ pet.element_name
          ,pet.attribute6            ded_contr_code
          ,substr(pcak.segment1,1,7) db_acct
          ,substr(pcak.segment13,1,5) db_sub_acct
          ,'2201'                    db_object
          ,'2462010'                 cr_acct
          ,null                      cr_sub_acct   -- schill - 432807
          ,'1627'
          ,sum(nvl(dd.ELEMENT_ENTRY_VALUE,0)) costed_value
         from pay_element_classifications pec
             ,pay_cost_allocation_keyflex pcak
             ,pay_element_entries_f  pee
             ,pay_element_links_f    pel
             ,pay_element_types_f    pet
             ,per_all_assignments_f  pas
             ,csuh_double_deductions dd
         where pcak.cost_allocation_keyflex_id = pel.cost_allocation_keyflex_id
           and pec.classification_id    = pet.classification_id
           and dd.ELEMENT_EFFECTIVE_DATE between pee.effective_start_date and pee.effective_end_date
           and pee.element_link_id      = pel.element_link_id
           and pee.assignment_id        = pas.assignment_id
           and dd.ELEMENT_EFFECTIVE_DATE between pel.effective_start_date and pel.effective_end_date
           and pel.element_type_id      = pet.element_type_id
           and pet.classification_id   in (29    -- Employer Taxes
                                          ,33    -- Employer Liabilities
                                           )
           and dd.ELEMENT_EFFECTIVE_DATE between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = dd.BASE_ELEMENT_ID
           and pas.payroll_id||''       = l_payroll
           and dd.ELEMENT_EFFECTIVE_DATE between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = dd.assignment_id
           and to_char(dd.BATCH_PROCESS_DATE,'mmyyyy') = to_char(l_end_date, 'mmyyyy')
           and l_run_type = 'R'
  group by pet.element_name
          ,pet.attribute6
          ,substr(pcak.segment1,1,7)
          ,substr(pcak.segment13,1,5)
          ,'2201'
          ,'2462010'
          ,'1627';


--*********************************************
--*  Third Party Checks
--*********************************************
 cursor get_third_party_lines is
    select /*+ RULE */ substr(pcak.segment1,1,7) asg_acct
          ,substr(pcak.segment13,1,5) asg_sub_acct  -- schill - 432807
          ,substr(pcak.segment2,1,4) object
          ,pet.element_name
          ,pet.attribute6            ded_contr_code
          ,sum(decode(pc.DEBIT_OR_CREDIT,'D',-1,1)*pc.costed_value) costed_value
          ,classification_name
         from pay_element_classifications pec
             ,pay_cost_allocation_keyflex pcak
             ,pay_element_types_f   pet
             ,per_all_assignments_f  pas
             ,pay_run_results   prr
             ,pay_costs    pc
             ,pay_assignment_actions  paa
             ,pay_payroll_actions    ppa
         where pec.classification_id    = pet.classification_id
           and pcak.cost_allocation_keyflex_id = pc.cost_allocation_keyflex_id
           and ppa.effective_date between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = paa.assignment_id
           and pet.THIRD_PARTY_PAY_ONLY_FLAG = 'Y'
           and ppa.effective_date between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = prr.element_type_id
           and prr.run_result_id         = pc.run_result_id
           and pc.balance_or_cost       = 'C'     -- debits only
           and pc.assignment_action_id  = paa.assignment_action_id
           and ((ppa.action_type = 'C' and CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = l_run_type)
             or (ppa.action_type = 'B' and l_run_type = 'R')
             or (ppa.action_type = 'C' and CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = '?' and l_run_type  = 'Q')
             or (ppa.action_type = 'C' and CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = 'V' and l_run_type  = 'Q')
               )
           and paa.payroll_action_id    = ppa.payroll_action_id
           and ppa.action_status = 'C'     -- complete
           and ppa.action_type in ('C','B')  -- costing
           and ppa.effective_date between l_start_date and l_end_date
           and ppa.payroll_id    = l_payroll
  group by pet.element_name
     ,classification_name
     ,pet.attribute6
     ,substr(pcak.segment1,1,7)
     ,substr(pcak.segment13,1,5)  -- schill - 432807
     ,substr(pcak.segment2,1,4);

--*--------------------------------------------------------------------*
--* Get the employees that have the Research Leave element for the 
--* current payroll period.
--*--------------------------------------------------------------------*
cursor get_emp is
select peo.full_name               empl_name
      ,ppa.action_type
      ,pas.assignment_number       pid
      ,pas.assignment_id
      ,substr(pj.name,1,6)         job_class      
      ,prrv.result_value           Hours
      ,to_date(prrv2.result_value,'YYYY/MM/DD HH24:MI:SS') RL_Date
      ,prrv3.result_value          Reason
      ,paa.assignment_action_id
      ,pro.proposed_salary_n       salary
      ,substr(pcak.segment1,1,7)   account#
      ,substr(pcak.segment2,1,4)   object
      ,substr(pcak.segment3,1,7)   fr_acct
from   
       per_pay_proposals           pro
      ,pay_cost_allocation_keyflex pcak
      ,pay_element_links_f         pel
      ,pay_element_types_f         pet
      ,per_all_people_f            peo
      ,per_jobs                    pj
      ,per_all_assignments_f       pas
      ,pay_input_values_f          piv3
      ,pay_run_result_values       prrv3
      ,pay_input_values_f          piv2
      ,pay_run_result_values       prrv2
      ,pay_input_values_f          piv
      ,pay_run_result_values       prrv
      ,pay_run_results             prr
      ,pay_assignment_actions      paa
      ,pay_payroll_actions         ppa
where  ppa.payroll_id              = l_payroll
and    CSUH_GET_PAYROLL_RUN_TYPE(paa.assignment_action_id) = 'R'      
and    l_run_type = 'R'
and    ppa.effective_date between  l_start_date and l_end_date
and    ppa.action_status           = 'C'     -- complete
and    paa.payroll_action_id       = ppa.payroll_action_id
and    prr.assignment_action_id    = paa.assignment_action_id
and    pet.element_type_id         = prr.element_type_id
and    prrv.run_result_id          = prr.run_result_id
and    prrv.result_value           <> '0'
and    piv.input_value_id          = prrv.input_value_id
and    piv.name                    = 'Hours'
and    prrv2.run_result_id         = prr.run_result_id
and    piv2.input_value_id         = prrv2.input_value_id
and    piv2.name                   = 'Date'
and    prrv3.run_result_id         = prr.run_result_id
and    piv3.input_value_id         = prrv3.input_value_id
and    piv3.name                   = 'Reason'
and    ppa.effective_date between pet.effective_start_date and pet.effective_end_date
and    pet.element_name            in ('Research Leave')
and    ppa.effective_date between pas.effective_start_date and pas.effective_end_date
and    pas.assignment_id           = paa.assignment_id
and    pj.job_id                   = pas.job_id
and    peo.person_id               = pas.person_id
and    ppa.effective_date between peo.effective_start_date and peo.effective_end_date
and    pel.element_type_id         = pet.element_type_id
and    pel.organization_id         = pas.organization_id
and    csu_utility.character_to_date(prrv2.result_value) between pel.effective_start_date and pel.effective_end_date
and    pcak.cost_allocation_keyflex_id = pel.cost_allocation_keyflex_id 
and    pro.change_date = (select max(ppp.change_date)
                          from   per_pay_proposals ppp
                          where  ppp.change_date <= csu_utility.character_to_date(prrv2.result_value)
                          and    ppp.assignment_id = pro.assignment_id)
and    pro.approved                = 'Y'
and    pro.assignment_id           = pas.assignment_id;

--*--------------------------------------------------------------------*
--* Get the account number and object code for the salary element(s)
--* where the distribution date is within the payroll period of the
--* input value date of the Research Leave element.
--*--------------------------------------------------------------------*

cursor salary_dist(p_assignment_id number) is 
select dlh.distribution_amount
      ,dlh.distribution_date  
      ,pcak.segment1    account#
      ,pcak.segment2    object 
      ,pcak.segment4    fringe_rate
      ,pcak.segment5    fringe_obj
      ,pet.element_name salary_name
      ,gcc.segment1     segment1
      ,gcc.segment2     segment2
from   pay_cost_allocation_keyflex    pcak
      ,gl_code_combinations           gcc
      ,csuh_distribution_interface    cdi
      ,psp_pre_gen_dist_lines_history dlh
      ,pay_element_types_f            pet
      ,pay_element_sets               pes
      ,pay_element_type_rules         petr
where  pes.element_set_name = 'CSU Research Leave Salary Elements'
and    petr.element_set_id     = pes.element_set_id
and    petr.include_or_exclude = 'I'
and    pet.element_type_id = petr.element_type_id
and    v_payroll_end_date between pet.effective_start_date and pet.effective_end_date
and    dlh.element_type_id = petr.element_type_id
and    dlh.assignment_id = p_assignment_id
and    dlh.distribution_date between v_payroll_start_date and v_payroll_end_date
and    dlh.reversal_entry_flag is null
and    cdi.distribution_interface_id = dlh.distribution_interface_id
and    gcc.code_combination_id = dlh.gl_code_combination_id
and    pcak.cost_allocation_keyflex_id = cdi.cost_allocation_keyflex_id;



--------------------------------------------------------------
-- Declare control variables
--------------------------------------------------------------

ctl_earnings_cnt           number          := 0;
ctl_deductions_cnt         number          := 0;
ctl_cash_cnt               number          := 0;
ctl_surcharge_cnt          number          := 0;
ctl_earnings_sc            number          := 0;
ctl_deductions_sc          number          := 0;
ctl_sum_salary             number          := 0;
ctl_sum_surcharge          number          := 0;
ctl_sum_fringe             number          := 0;
ctl_sum_cpp                number          := 0;
ctl_cpp_cnt                number          := 0;
ctl_sum_ee_deductions      number          := 0;
ctl_sum_cash_deductions    number          := 0;
ctl_sum_er_deductions      number          := 0;
ctl_sum_3rd_party_checks   number          := 0;
ctl_sum_non_payroll        number          := 0;
ctl_cnt_non_payroll        number          := 0;
l_batch_count              number          := 0;
l_batch_total_amount       number          := 0;
l_earnings_temp            number          := 0;
l_empl_cd                  varchar2(3);

--------------------------------------------------------------
-- program variables
--------------------------------------------------------------
l_balance               number := 0;
l_asg_acct              varchar2(7) := null;
l_object                varchar2(4) := null;
l_fr_object             varchar2(4) := null;
l_fr_amount             number      := 0;
l_fr_rate               number      := 0;
l_cpp_amount            number      := 0;
l_cpp_fr_amount         number      := 0;
l_full_name             varchar2(255);
l_partial_fringe        varchar2(1);
l_eff_date              date;
null_params             exception;
kfs_not_balanced        exception;
l_subaccount            varchar2(5);

l_fr_account            varchar2(10) := null;
l_chart_cd              varchar2(2)  := null;
l_cpp_account           varchar2(10) := null;
l_total_summary_amount  number       := 0;
l_double_deduction_inc  number       := 0;
l_double_deduction_exc  number       := 0;
l_total_fringe_amount   number       := 0;
l_total_deductions_amount number     := 0;
l_summary_deduction_amount number    := 0;
l_net                   number       := 0;
l_bal_id1               number       := 0;
l_bal_id2               number       := 0;

l_credit_acct_salary      varchar2(11); 
l_credit_acct_surcharge   varchar2(11);
l_credit_acct_fringe      varchar2(11);
l_credit_acct_cpp         varchar2(11);
l_credit_acct_912         varchar2(11);
l_credit_acct_912_hold    varchar2(11);
l_credit_acct_deduction   varchar2(11);
l_credit_sub_acct_deduction   varchar2(10);
l_credit_object_deduction varchar2(4);
l_debit_acct_deduction    varchar2(10) := '0000000';
l_debit_object_deduction  varchar2(4):= '0000';
l_amount number;
l_netpay_field            varchar2(10);
l_bank_field              varchar2(10);
v_debit                   varchar2(1);
v_credit                  varchar2(1);
l_current_month varchar2(3);
l_current_year varchar2(4);
v_fiscal_period varchar2(2);
v_credit_acct_salary      varchar2(11);
v_amount                  number;
v_rate                    number;
v_total_salary            number;

-- schill - 1286777
v_chart_cd_debit_deduct   varchar2(10);
v_chart_cd_credit_deduct   varchar2(10);


Procedure get_credit_globals (p_date  IN Date
                             ,p_credit_acct_salary    IN OUT varchar2
                             ,p_credit_acct_surcharge IN OUT varchar2
                             ,p_credit_acct_fringe    IN OUT varchar2
                             ,p_credit_acct_cpp       IN OUT varchar2
                             ,p_credit_acct_912       IN OUT varchar2
                             ,p_credit_acct_912_hold  IN OUT varchar2) IS

Cursor C1 (l_date date) is
   select /*+ RULE */ global_name, global_value
   from ff_globals_f
   where l_date between EFFECTIVE_START_DATE and EFFECTIVE_END_DATE
   and global_name like 'KFS%';
Begin

  For G in C1 (p_date) Loop
    If    G.global_name = 'KFS_912_ACCOUNT'       Then
       p_credit_acct_912    := G.global_value;
    Elsif G.global_name = 'KFS_912_HOLD_ACCOUNT'  Then
       p_credit_acct_912_hold := G.global_value;
    Elsif G.global_name = 'KFS_CPP_ACCOUNT'       Then
       p_credit_acct_cpp    := G.global_value;
    Elsif G.global_name = 'KFS_EARNINGS_ACCOUNT'  Then
       p_credit_acct_salary := G.global_value;
    Elsif G.global_name = 'KFS_FRINGE_ACCOUNT'    Then
       p_credit_acct_fringe := G.global_value;
    Elsif G.global_name = 'KFS_SURCHARGE_ACCOUNT' Then
       p_credit_acct_surcharge := G.global_value;
    End if;
  End Loop;
End;


-- ************************************************
-- GET Classification
-- ************************************************
Function get_classification (p_element_name varchar2
                            ,p_date         date) return varchar2 is
   v_name  varchar2(255);
begin
  select /*+ RULE */ substr(CLASSIFICATION_NAME,1,7) into v_name
  from PAY_ELEMENT_CLASSIFICATIONS pec
      ,pay_element_types_f pet
  where pec.CLASSIFICATION_ID = pet.CLASSIFICATION_ID
    and p_date between pet.effective_start_date and pet.effective_end_date
    and pet.element_name = p_element_name;

    return(nvl(v_name,'???'));
exception
  when no_data_found Then
      return('???');
end;


--***********************
--*  Get fringe account
--***********************
procedure get_fringe_account (p_account IN varchar2, p_frr_acct IN varchar2, 
                              v_account OUT varchar2, v_chart_cd OUT varchar2) is
begin
   if p_frr_acct is null then
      begin
         select decode(fringe_account_nbr,'0000000',account_nbr,fringe_account_nbr),
                chart_cd
         into   v_account, 
                v_chart_cd
         from   csuf_accounts
         where  account_nbr = p_account;
      end;
   else
      begin
         v_account := p_frr_acct;
         select chart_cd
         into   v_chart_cd
         from   csuf_accounts
         where  account_nbr = p_frr_acct;
      end;
   end if;

exception when no_data_found then
   v_account := p_account;
   v_chart_cd := 'CO';
end;


-- ********************
-- *  Earnings Record
-- ********************
procedure earnings_record(out_file      in utl_file.file_type,
                          p_chart_cd    in varchar2,
                          p_account     in varchar2,
                          p_segment2    in varchar2,
                          p_object      in varchar2,
                          p_pid         in varchar2,
                          p_eff_date    in date,
                          p_empl_name   in varchar2,
                          p_amount      in number,
                          p_credit_acct in varchar2,
                          p_job_class   in varchar2) is

v_debit    varchar2(1);
v_credit   varchar2(1);

BEGIN

If round(p_amount,2) = 0 Then
  NULL;
ELSE
  If round(p_amount,2) >= 0 Then
    v_amt := p_amount;
    v_debit  := 'D';
    v_credit := 'C';
  else 
    v_amt := p_amount * -1;
    v_debit  := 'C';
    v_credit := 'D';
  end if;

  utl_file.put_line(out_file,
        l_fiscal_year||
        rpad(p_chart_cd,2)||
        rpad(nvl(p_account,' '),7,' ')||
        rpad(nvl(p_segment2,' '),5,' ')||
        rpad(nvl(p_object,' '),4,' ')||
        '   '||
        'AC'||
        '  '||
        rpad(nvl(v_fiscal_period,' '),2,' ')|| 
        'PAYD'||
        l_origin_code||
        rpad(p_pid,14)||
        '     '||
        rpad(substr(p_empl_name,1,40),40)||
        ltrim(to_char(v_amt,'09999999999999999.99'))||
        v_debit||
        to_char(sysdate,'YYYY-MM-DD')||
        to_char(p_eff_date,'YYYY-MM-DD')||      -- org ref id
        '          '||                          -- project
        rpad(nvl(p_job_class,' '),8,' ')||      --
        rpad(' ',39));

  utl_file.put_line(out_file,
        l_fiscal_year||
        'CO'||
        rpad(substr(p_credit_acct,1,7),7,' ')||
        rpad(' ',5)||
        rpad(substr(p_credit_acct,8,11),4,' ')||
        '   '||
        'AC'||
        '  '||
        rpad(nvl(v_fiscal_period,' '),2,' ')|| 
        'PAYD'||
        l_origin_code||
        rpad(p_pid,14)||
        '     '||
        rpad(substr(p_empl_name,1,40),40)||
        ltrim(to_char(v_amt,'09999999999999999.99'))||
        v_credit||
        to_char(sysdate,'YYYY-MM-DD')||
        to_char(p_eff_date,'YYYY-MM-DD')||      -- org ref id
        '          '||                          -- project
        rpad(nvl(p_job_class,' '),8,' ')||      --
        rpad(' ',39));

  l_batch_count := l_batch_count + 2;
  l_batch_total_amount := l_batch_total_amount + (round(v_amt,2) * 2);
END IF;

END;

-- ********************
-- *  Update Research Leave table
-- ********************
procedure Update_research_leave_table(
                          p_chart_cd    in varchar2,
                          p_account     in varchar2,
                          p_segment2    in varchar2,
                          p_object      in varchar2,
                          p_pid         in varchar2,
                          p_eff_date    in date,
                          p_empl_name   in varchar2,
                          p_amount      in number,
                          p_credit_acct in varchar2,
                          p_job_class   in varchar2) is


BEGIN

If round(p_amount,2) = 0 Then
  NULL;
ELSE
  If round(p_amount,2) >= 0 Then
    v_amt := p_amount;
    v_debit  := 'D';
    v_credit := 'C';
  else 
    v_amt := p_amount * -1;
    v_debit  := 'C';
    v_credit := 'D';
  end if;

   insert into csuh_research_leave_00                 
              values (sysdate,
                      l_research_seq,
                      p_chart_cd,
                      p_account,
                      p_segment2,
                      p_object,
                      p_pid,
                      p_eff_date,
                      p_empl_name,
                      v_debit,
                      v_amt,
                      p_job_class);
   l_RL_count := l_RL_count + 1;

   insert into csuh_research_leave_00                 
              values (sysdate,
                      l_research_seq,
                      p_chart_cd,
                      substr(p_credit_acct,1,7),
                      '    ',
                      substr(p_credit_acct,8,11),
                      p_pid,
                      p_eff_date,
                      p_empl_name,
                      v_credit,
                      v_amt,
                      p_job_class);
   l_RL_count := l_RL_count + 1;
end if;
End;


--*****************************************************************************
--*   main                                                                    *
--*****************************************************************************

BEGIN

dbms_output.enable(1000000);
DBMS_OUTPUT.PUT_LINE
   ('**** Start of HRMSS175 ' ||to_char(sysdate,'MM/DD/YYYY HH24:MI:SS'));

select to_char(sysdate,'MON'), to_char(sysdate,'YYYY')
into l_current_month, l_current_year
from dual;

select research_leave_seq.NEXTVAL, 
sysdate 
into l_research_seq, l_todays_date from dual;


if l_current_month = 'JUL' and l_fiscal_year = l_current_year then 
  v_fiscal_period := '13';
else
  v_fiscal_period := '  ';
end if;
 
if out_lis1 is null or out_lis2 is null or out_dir is null or l_start_date is null or l_end_date is null
     or l_payroll is null or l_run_type is null or l_fiscal_year is null then
  raise null_params;
end if;

out_file1 := utl_file.fopen(out_dir,out_lis1,'w');
out_file2 := utl_file.fopen(out_dir,out_lis2,'w');

if l_payroll = 21 then 
  l_origin_code := 'P4';
elsif l_payroll = 22 then 
  l_origin_code := 'P3';
end if;

-- ******************************************
-- * Get values for credit account/objects
-- ******************************************
get_credit_globals (l_end_date
                   ,l_credit_acct_salary
                   ,l_credit_acct_surcharge
                   ,l_credit_acct_fringe
                   ,l_credit_acct_cpp
                   ,l_credit_acct_912
                   ,l_credit_acct_912_hold);

-- *******************************
-- * sum Net Record
-- *******************************
DBMS_OUTPUT.PUT_LINE('. start of net rec     ' ||to_char(sysdate,'MM/DD/YYYY HH24:MI:SS'));
  pay_balance_pkg.set_context('TAX_UNIT_ID','0');
  l_bal_id1 := hr_us_reports.get_defined_balance_id('Net','_ASG_GRE_RUN',0);
  l_bal_id2 := hr_us_reports.get_defined_balance_id('Deduction Adjustment','_ASG_GRE_RUN',0);
  for c10 in net_records (l_start_date, l_end_date, l_payroll) loop
     l_net := l_net + nvl(pay_balance_pkg.get_value(l_bal_id1, C10.run_no),0);
  end loop;

If l_run_type = 'R' and l_payroll = 99 Then
   select /*+ RULE */ sum(nvl(dd.ELEMENT_ENTRY_VALUE,0)) into l_double_deduction_inc
         from pay_element_classifications pec
             ,pay_element_entries_f  pee
             ,pay_element_links_f    pel
             ,pay_element_types_f    pet
             ,per_all_assignments_f  pas
             ,csuh_double_deductions dd
         where pec.classification_id in (28      -- Involuntary Deductions
                                        ,30      -- Pre-Tax Deductions
                                        ,31      -- Tax Deductions
                                        ,32      -- Voluntary Deductions
                                        ,35      -- Non-payroll Payments
                                        ,36      -- Tax Credits
                                        )
           and pec.classification_id    = pet.classification_id
           and dd.ELEMENT_EFFECTIVE_DATE between pee.effective_start_date and pee.effective_end_date
           and pee.element_link_id      = pel.element_link_id
           and pee.assignment_id        = pas.assignment_id
           and dd.ELEMENT_EFFECTIVE_DATE between pel.effective_start_date and pel.effective_end_date
           and pel.element_type_id      = pet.element_type_id
           and dd.ELEMENT_EFFECTIVE_DATE between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = dd.BASE_ELEMENT_ID
           and pas.payroll_id||''       = l_payroll
           and dd.ELEMENT_EFFECTIVE_DATE between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = dd.assignment_id
           and to_char(dd.BATCH_PROCESS_DATE,'mmyyyy') = to_char(l_end_date, 'mmyyyy');

   select /*+ RULE */ sum(nvl(dd.ELEMENT_ENTRY_VALUE,0)) into l_double_deduction_exc
         from pay_element_classifications pec
             ,pay_element_entries_f  pee
             ,pay_element_links_f    pel
             ,pay_element_types_f    pet
             ,per_all_assignments_f  pas
             ,csuh_double_deductions dd
         where pec.classification_id in (28      -- Involuntary Deductions
                                        ,30      -- Pre-Tax Deductions
                                        ,31      -- Tax Deductions
                                        ,32      -- Voluntary Deductions
                                        ,35      -- Non-payroll Payments
                                        ,36      -- Tax Credits
                                        )
           and pec.classification_id    = pet.classification_id
           and dd.ELEMENT_EFFECTIVE_DATE between pee.effective_start_date and pee.effective_end_date
           and pee.element_link_id      = pel.element_link_id
           and pee.assignment_id        = pas.assignment_id
           and dd.ELEMENT_EFFECTIVE_DATE between pel.effective_start_date and pel.effective_end_date
           and pel.element_type_id      = pet.element_type_id
           and dd.ELEMENT_EFFECTIVE_DATE between pet.effective_start_date and pet.effective_end_date
           and pet.element_type_id      = dd.BASE_ELEMENT_ID
           and pas.payroll_id||''       = l_payroll
           and dd.ELEMENT_EFFECTIVE_DATE between pas.effective_start_date and pas.effective_end_date
           and pas.assignment_id        = dd.assignment_id
           and to_char(dd.ELEMENT_EFFECTIVE_DATE,'mmyyyy') = to_char(l_end_date, 'mmyyyy');
End if;

l_net := l_net - nvl(l_double_deduction_inc,0) + nvl(l_double_deduction_exc,0);

--*********************************************
--* earnings transactions
--*********************************************
DBMS_OUTPUT.PUT_LINE('. start of earnings    ' ||to_char(sysdate,'MM/DD/YYYY HH24:MI:SS'));

For C2 in get_pre_gen_amounts (l_start_date, l_end_date) Loop
 If C2.payroll_id = l_payroll Then
      l_partial_fringe := C2.partial_fringe;
      l_empl_cd  := C2.EARNINGS_TYPE;
      l_eff_date := c2.effective_date;

   If l_partial_fringe = 'Y'
     and c2.partial_fr_earnings_object is not null
     and c2.partial_fr_object          is not null
     and c2.partial_fr_rate            is not null Then
       l_object    := c2.partial_fr_earnings_object;
       l_fr_object := c2.partial_fr_object;
       l_fr_amount := c2.summary_amount * c2.partial_fr_rate/100;
       l_fr_rate   := c2.partial_fr_rate;
   Else
       l_object    := c2.object;
       l_fr_object := c2.fr_object;
       l_fr_amount := c2.fr_rate * c2.summary_amount/100;
       l_fr_rate   := c2.fr_rate;
   End If;

   If  C2.element_name = 'Regular Salary 9 Month'
    and C2.flag_9over12 = 'Y' Then
       If to_char(l_end_date,'mm') in ('06','07','08') Then
          l_object := substr(l_credit_acct_912,8,4);         -- 2401
       End if;
   End if;

   l_asg_acct := nvl(c2.segment1,c2.asg_acct);

   get_fringe_account(l_asg_acct, c2.fr_acct, l_fr_account, l_chart_cd);

   If l_fr_account = l_asg_acct Then
      l_subaccount := nvl(c2.segment2,'-----');
   Else
      l_subaccount := '-----';
   End if;

   If  C2.element_name IN ('Surcharge Work Study', 'Surcharge Work Study Summer') Then
         earnings_record(out_file1, l_chart_cd, l_asg_acct, c2.segment2, l_object, c2.pid, l_eff_date,
                    c2.empl_name, c2.summary_amount, l_credit_acct_surcharge, c2.job_class);
         ctl_surcharge_cnt := ctl_surcharge_cnt + 1;
         ctl_sum_surcharge := ctl_sum_surcharge + round(c2.summary_amount,2);
   ElsIf  C2.element_name = 'Performance Pay Norecover'
      and nvl(csuh_fap_assess_rate(C2.assignment_id, 66, C2.effective_date), 0) <> 0 Then
         earnings_record(out_file1, l_chart_cd, l_asg_acct, c2.segment2, l_object, c2.pid, l_eff_date,
                    c2.empl_name, c2.summary_amount, l_credit_acct_cpp, c2.job_class);
         ctl_cpp_cnt   := ctl_cpp_cnt + 1;
         ctl_sum_cpp   := ctl_sum_cpp + round(c2.summary_amount,2);
   Else
         earnings_record(out_file1, l_chart_cd, l_asg_acct, c2.segment2, l_object, c2.pid, l_eff_date,
                    c2.empl_name, c2.summary_amount, l_credit_acct_salary, c2.job_class);
         ctl_earnings_cnt := ctl_earnings_cnt + 1;
         ctl_sum_salary   := ctl_sum_salary + round(c2.summary_amount,2);
   End if;

   If round(c2.summary_amount,2) <> 0 Then
      l_total_summary_amount := l_total_summary_amount + (abs(round(c2.summary_amount,2)) * 2);
   End if;

   If  C2.rec_type = 'NONPAYROLL' Then
       l_net := l_net + round(c2.summary_amount,2);
       ctl_cnt_non_payroll := ctl_cnt_non_payroll  + 1;
       ctl_sum_non_payroll := ctl_sum_non_payroll  + round(c2.summary_amount,2);
   End if;

   If nvl(l_fr_amount,0) <> 0 Then
      If to_char(l_end_date,'mm') in ('06','07','08')
        and C2.element_name = 'Regular Salary 9 Month'
        and C2.flag_9over12 = 'Y' Then
         earnings_record(out_file1, l_chart_cd, substr(l_credit_acct_912_hold,1,7), '', 
                         substr(l_credit_acct_912_hold,8,4), c2.pid, l_eff_date,
                         c2.empl_name, l_fr_amount, l_credit_acct_fringe, c2.job_class);
         If round(l_fr_amount,2) <> 0 Then
            ctl_earnings_sc  := ctl_earnings_sc + 1;
            ctl_sum_fringe   := ctl_sum_fringe + nvl(round(l_fr_amount,2),0);
            l_total_summary_amount := l_total_summary_amount + (abs(nvl(round(l_fr_amount,2),0)) * 2);
         End if;
      Else
         get_fringe_account(l_asg_acct, c2.fr_acct, l_fr_account, l_chart_cd);
         If l_fr_account = l_asg_acct Then
            l_subaccount := nvl(c2.segment2,'-----');
         Else
            l_subaccount := '-----';
         End if;
         If l_fr_account is not null Then
            earnings_record(out_file1, l_chart_cd, l_fr_account, l_subaccount, l_fr_object, c2.pid, l_eff_date,
                            c2.empl_name, l_fr_amount, l_credit_acct_fringe, c2.job_class);
            If round(l_fr_amount,2) <> 0 Then
               ctl_earnings_sc  := ctl_earnings_sc + 1;
               ctl_sum_fringe   := ctl_sum_fringe + nvl(round(l_fr_amount,2),0);
               l_total_summary_amount := l_total_summary_amount + (abs(nvl(round(l_fr_amount,2),0)) * 2);
            End if;
         Else
            DBMS_OUTPUT.PUT_LINE('No Fringe Account: '||l_asg_acct||' '||rpad(c2.empl_name,25)||' '||
                                 rpad(c2.element_name,25)||' '||to_char(l_fr_amount));
         End if;
      End if;
   End if;

   l_cpp_amount  := round(c2.cpp_rate * c2.summary_amount/100,2);
   If nvl(l_cpp_amount,0) <> 0 Then
      get_fringe_account(l_asg_acct, c2.fr_acct, l_fr_account, l_chart_cd);
      If C2.payroll_id = 21 Then
         If C2.people_group_id = 31 Then        -- State Class
           l_empl_cd := 'PPS';
         Else
           l_empl_cd := 'PPR';
           If l_cpp_account like '13019%' Then
              l_cpp_account := '130189';
           End if;
         End if;
      else
        l_empl_cd := 'PPH';
      End if;
      If l_cpp_account is not null Then
           If l_empl_cd = 'PPR' Then
              earnings_record(out_file1, l_chart_cd, l_cpp_account, '', l_object, c2.pid, l_eff_date,
                           c2.empl_name, l_cpp_amount, l_credit_acct_cpp, c2.job_class);
           Else
              earnings_record(out_file1, l_chart_cd, l_cpp_account, '', C2.cpp_object, c2.pid, l_eff_date,
                           c2.empl_name, l_cpp_amount, l_credit_acct_cpp, c2.job_class);
           End if;
           If round(l_cpp_amount,2) <> 0 Then
              ctl_cpp_cnt   := ctl_cpp_cnt + 1;
              ctl_sum_cpp   := ctl_sum_cpp + l_cpp_amount;
              l_total_summary_amount := l_total_summary_amount + (abs(l_cpp_amount) * 2);
           End if;
      Else
            DBMS_OUTPUT.PUT_LINE('No CPP Account: '||l_asg_acct||' '||rpad(c2.empl_name,25)||' '||
                                 rpad(c2.element_name,25)||' '||to_char(l_cpp_amount));
      End if;
   End if;

   l_cpp_fr_amount  := round(c2.cpp_rate * l_fr_amount/100,2);
   If nvl(l_cpp_fr_amount,0) <> 0 Then
      get_fringe_account(l_asg_acct, c2.fr_acct, l_fr_account, l_chart_cd);
      If C2.payroll_id = 21 Then
         If C2.people_group_id = 31 Then        -- State Class
           l_empl_cd := 'PPS';
         Else
           l_empl_cd := 'PPR';
         End if;
      else
        l_empl_cd := 'PPH';
      End if;
      If l_fr_account = l_asg_acct Then
         l_subaccount := nvl(c2.segment2,'-----');
      Else
         l_subaccount := '-----';
      End if;
      If l_fr_account is not null Then
         If l_empl_cd = 'PPR' Then
            earnings_record(out_file1, l_chart_cd, l_fr_account, l_subaccount, l_fr_object, c2.pid, l_eff_date,
                            c2.empl_name, l_cpp_fr_amount, l_credit_acct_fringe, c2.job_class);
         Else
            earnings_record(out_file1, l_chart_cd, l_fr_account, l_subaccount, c2.cpp_fr_object, c2.pid, l_eff_date,
                            c2.empl_name, l_cpp_fr_amount, l_credit_acct_fringe, c2.job_class);
         End if;
         If round(l_cpp_fr_amount,2) <> 0 Then
            ctl_cpp_cnt   := ctl_cpp_cnt + 1;
            ctl_sum_cpp   := ctl_sum_cpp + nvl(round(l_cpp_fr_amount,2),0);
            l_total_summary_amount := l_total_summary_amount + (abs(nvl(round(l_cpp_fr_amount,2),0)) * 2);
         End if;
      Else
            DBMS_OUTPUT.PUT_LINE('No Fringe Account: '||l_asg_acct||' '||rpad(c2.empl_name,25)||' '||
                                 rpad(c2.element_name,25)||' '||to_char(l_fr_amount));
      End if;
   End if;
 End if;

End Loop;   --C2

BEGIN
  Select nvl(max(date_earned), l_end_date) into l_eff_date
      from pay_payroll_actions
     where action_type    = l_run_type
       and payroll_id     = l_payroll
       and effective_date = l_end_date;
Exception
  When no_data_found Then
     l_eff_date := trunc(sysdate);
End;


If l_run_type = 'R' Then
-- *************************************
-- *  get Cash Payments
-- *************************************
DBMS_OUTPUT.PUT_LINE('. start of cash payment' ||to_char(sysdate,'MM/DD/YYYY HH24:MI:SS'));
  For CP in get_cash_payment_lines Loop
      l_partial_fringe := 'N';
      l_empl_cd := csuh_earnings_type(CP.assignment_id, l_end_date, Null);

      earnings_record(out_file1, l_chart_cd, CP.asg_acct, '', '2201', CP.assignment_number, l_eff_date,
                    CP.emp_name, CP.costed_value, l_credit_acct_salary, CP.job_class);

      If round(CP.costed_value,2) <> 0 Then
         ctl_earnings_cnt := ctl_earnings_cnt + 1;
         ctl_sum_salary   := ctl_sum_salary + round(CP.costed_value,2);
         l_total_summary_amount := l_total_summary_amount + (abs(round(CP.costed_value,2)) * 2);
   End if;

  End Loop;  -- CP
End if;

-- *************************************
-- *  get non ER liability deductions
-- *************************************
-- get costing, first record is all we need, same for every individual element
DBMS_OUTPUT.PUT_LINE('. start of deduct      ' ||to_char(sysdate,'MM/DD/YYYY HH24:MI:SS'));
  for c3 in get_deductions_summary_lines loop
     if c3.costed_value <> 0 then
       l_credit_acct_deduction := c3.asg_acct;
       l_credit_sub_acct_deduction := c3.asg_sub_acct;  -- schill - 432807
       l_credit_object_deduction := c3.object;
       l_debit_acct_deduction := substr(l_credit_acct_salary,1,7);       -- '2462010';

       If c3.line_type = 'DD' Then
          l_debit_object_deduction := '2201';                            -- Prepaid Insur
       Else
          l_debit_object_deduction := substr(l_credit_acct_salary,8,4);  -- '1620';
       End if;
       If  l_credit_acct_deduction  = '2463000' Then        -- Cash Payment
          ctl_cash_cnt := ctl_cash_cnt + 1;
          ctl_sum_cash_deductions := ctl_sum_cash_deductions + round(c3.costed_value,2);
          l_credit_acct_deduction := '2462010';
          l_credit_sub_acct_deduction := null;  -- schill - 432807
       Elsif c3.line_type = 'DD' Then
          ctl_deductions_cnt := ctl_deductions_cnt + 1;
          If c3.costed_value > 0 Then
             l_double_deduction_inc := l_double_deduction_inc + round(c3.costed_value,2);
          Else
             l_double_deduction_exc := l_double_deduction_exc + abs(round(c3.costed_value,2));
          End if;
       Else
          ctl_deductions_cnt := ctl_deductions_cnt + 1;
          ctl_sum_ee_deductions := ctl_sum_ee_deductions + round(c3.costed_value,2);
       End if;
 
       If c3.costed_value >= 0 Then
         v_amt    := c3.costed_value;
         v_debit  := 'D';
         v_credit := 'C';
       else
         v_amt := c3.costed_value * -1;
         v_debit  := 'C';
         v_credit := 'D';
       end if;
       
       -- schill - 1286777
       select chart_cd
       into v_chart_cd_debit_deduct
       from csuf_accounts
       where account_nbr = rpad(nvl(substr(l_debit_acct_deduction,1,7),' '),7,' ')
       and inactive_flag = 'N';
 
-- schill - 432807
       utl_file.put_line(out_file1,
           l_fiscal_year||
           v_chart_cd_debit_deduct|| --'CO'||  -- schill - 1286777
           rpad(nvl(substr(l_debit_acct_deduction,1,7),' '),7,' ')||
           rpad(' ',5)||
           rpad(nvl(l_debit_object_deduction,' '),4,' ')||
           '   '||
           'AC'||
           '  '||
           rpad(nvl(v_fiscal_period,' '),2,' ')|| 
           'PAYD'||
           l_origin_code||
           'PAYROLL-DEDUCT'||
           '     '||
           rpad(substr(c3.element_name,1,40),40)||
           ltrim(to_char(round(v_amt,2),'09999999999999999.99'))||
           v_debit||
           to_char(sysdate,'YYYY-MM-DD')||
           to_char(l_eff_date,'YYYY-MM-DD')||      -- org ref id
           rpad(' ',49));
           
       -- schill - 1286777
       select chart_cd
       into v_chart_cd_credit_deduct
       from csuf_accounts
       where account_nbr = rpad(nvl(substr(l_credit_acct_deduction,1,7),' '),7,' ')
       and inactive_flag = 'N';

       utl_file.put_line(out_file1,
           l_fiscal_year||
           v_chart_cd_credit_deduct|| --'CO'||  -- schill - 1286777
           rpad(nvl(substr(l_credit_acct_deduction,1,7),' '),7,' ')||
           rpad(nvl(substr(l_credit_sub_acct_deduction,1,5),' '),5,' ')||  -- schill - 432807
           --rpad(' ',5)||
           rpad(nvl(l_credit_object_deduction,' '),4,' ')||
           '   '||
           'AC'||
           '  '||
           rpad(nvl(v_fiscal_period,' '),2,' ')|| 
           'PAYD'||
           l_origin_code||
           'PAYROLL-DEDUCT'||
           '     '||
           rpad(substr(c3.element_name,1,40),40)||
           ltrim(to_char(round(v_amt,2),'09999999999999999.99'))||
           v_credit||
           to_char(sysdate,'YYYY-MM-DD')||
           to_char(l_eff_date,'YYYY-MM-DD')||      -- org ref id
           rpad(' ',49));

       l_batch_count := l_batch_count + 2;
       l_batch_total_amount := l_batch_total_amount + (round(v_amt,2) * 2);

       If  C3.classification_name = 'Non-payroll Payments' Then
           l_net := l_net - round(c3.costed_value,2);
       End if;
       If c3.element_name = 'EIC ADJUSTMENT' then              -- Added 3-25-08
         l_net := l_net - round(c3.costed_value,2);
       end if;
       l_total_summary_amount := l_total_summary_amount + (abs(round(c3.costed_value,2)) * 2);
     end if;
     l_credit_acct_deduction := null;
     l_credit_sub_acct_deduction := null;  -- schill - 432807
     l_credit_object_deduction := null;
     l_debit_acct_deduction := null;
     l_debit_object_deduction := null;
  end loop; --c3


-- *************************************
-- *  get ER liability deductions
-- *************************************
-- get costing, first record is all we need, same for every individual element

DBMS_OUTPUT.PUT_LINE('. start of ER deduct   ' ||to_char(sysdate,'MM/DD/YYYY HH24:MI:SS'));
  for c4 in get_ded_er_summary_lines loop

     if c4.costed_value <> 0 then
       If c4.costed_value >= 0 Then
         v_amt := c4.costed_value;
         v_debit  := 'D';
         v_credit := 'C';
       else
         v_amt := c4.costed_value * -1;
         v_debit  := 'C';
         v_credit := 'D';
       end if;

-- schill - 432807
       utl_file.put_line(out_file1,
           l_fiscal_year||
           'CO'||
           rpad(nvl(c4.cr_acct,' '),7,' ')||
           rpad(nvl(c4.cr_sub_acct,' '),5,' ')||  -- schill - 432807
           --rpad(' ',5)||
           rpad(nvl(c4.cr_object,' '),4,' ')||
           '   '||
           'AC'||
           '  '||
           rpad(nvl(v_fiscal_period,' '),2,' ')|| 
           'PAYD'||
           l_origin_code||
           'PAYROLL-EMPLIA'||
           '     '||
           rpad(substr(c4.element_name,1,40),40)||
           ltrim(to_char(v_amt,'09999999999999999.99'))||
           v_credit||
           to_char(sysdate,'YYYY-MM-DD')||
           to_char(l_eff_date,'YYYY-MM-DD')||      -- org ref id
           rpad(' ',49));

       utl_file.put_line(out_file1,
           l_fiscal_year||
           'CO'||
           rpad(nvl(c4.db_acct,' '),7,' ')||
           rpad(nvl(c4.db_sub_acct,' '),5,' ')||  -- schill - 432807
           --rpad(' ',5)||
           rpad(nvl(c4.db_object,' '),4,' ')||
           '   '||
           'AC'||
           '  '||
           rpad(nvl(v_fiscal_period,' '),2,' ')|| 
           'PAYD'||
           l_origin_code||
           'PAYROLL-EMPLIA'||
           '     '||
           rpad(substr(c4.element_name,1,40),40)||
           ltrim(to_char(v_amt,'09999999999999999.99'))||
           v_debit||
           to_char(sysdate,'YYYY-MM-DD')||
           to_char(l_eff_date,'YYYY-MM-DD')||      -- org ref id
           rpad(' ',49));

       l_batch_count := l_batch_count + 2;
       l_batch_total_amount := l_batch_total_amount + (round(v_amt,2) * 2);

       ctl_deductions_sc := ctl_deductions_sc + 1;
       ctl_sum_er_deductions := ctl_sum_er_deductions + round(c4.costed_value,2);
       l_total_summary_amount := l_total_summary_amount + (abs(round(c4.costed_value,2)) * 2);
     end if;
     l_credit_acct_deduction   := null;
     l_credit_sub_acct_deduction := null;  -- schill - 432807
     l_credit_object_deduction := null;
     l_debit_acct_deduction    := null;
     l_debit_object_deduction  := null;
  end loop; --c4

-- *************************************
-- *  get Third Party Checks
-- *************************************
-- get costing, first record is all we need, same for every individual element
DBMS_OUTPUT.PUT_LINE('. start of third party ' ||to_char(sysdate,'MM/DD/YYYY HH24:MI:SS'));

  for c5 in get_third_party_lines loop
     if c5.costed_value <> 0 then
       If c5.costed_value < 0 Then
          l_netpay_field := 'C';
          l_bank_field   := 'D';
       Else
          l_netpay_field := 'D';
          l_bank_field   := 'C';
       End if;

-- schill - 432807
       utl_file.put_line(out_file1,
        l_fiscal_year||
        'CO'||
        rpad(nvl(c5.asg_acct,' '),7,' ')||
        rpad(nvl(c5.asg_sub_acct,' '),5,' ')||  -- schill - 432807
        --rpad(' ',5)||
        rpad(nvl(c5.object,' '),4,' ')||
        '   '||
        'AC'||
        '  '||
        rpad(nvl(v_fiscal_period,' '),2,' ')|| 
        'PAYD'||
        l_origin_code||
        'PAYROLL-TPC   '||
        '     '||
        rpad(substr(c5.element_name,1,40),40)||
        ltrim(to_char(abs(round(c5.costed_value,2)),'09999999999999999.99'))||
        l_netpay_field||
        to_char(sysdate,'YYYY-MM-DD')||
        to_char(l_eff_date,'YYYY-MM-DD')||      -- org ref id
        rpad(' ',39));

        l_batch_count := l_batch_count + 1;
        l_batch_total_amount := l_batch_total_amount + abs(round(c5.costed_value,2));

    utl_file.put_line(out_file1,
      l_fiscal_year||
      'CO'||
      '0000500'||    -- Bank 05
      rpad(' ',5)||
      '1103'||
      '   '||
      'AC'||
      '  '||
      rpad(nvl(v_fiscal_period,' '),2,' ')|| 
      'PAYD'||
      l_origin_code||
      'PAYROLL-BANK05'||
      '     '||
      rpad(substr(c5.element_name,1,40),40)||
      ltrim(to_char(abs(round(c5.costed_value,2)),'09999999999999999.99'))||
      l_bank_field||
      to_char(sysdate,'YYYY-MM-DD')||
      to_char(l_eff_date,'YYYY-MM-DD')||      -- org ref id
      rpad(' ',49));

      l_batch_count := l_batch_count + 1;
        l_batch_total_amount := l_batch_total_amount + abs(round(c5.costed_value,2));
 
        ctl_deductions_cnt := ctl_deductions_cnt + 1;
        ctl_sum_3rd_party_checks := ctl_sum_3rd_party_checks + abs(round(c5.costed_value,2));
        l_total_summary_amount := l_total_summary_amount + abs(round(c5.costed_value,2));
     End if;
  end loop; --c5

--*--------------------------------------------------------------------*
--* Create Research Leave transactions.
--*--------------------------------------------------------------------*
DBMS_OUTPUT.PUT_LINE('. start of Research Leave ' ||to_char(sysdate,'MM/DD/YYYY HH24:MI:SS'));

For emp in get_emp LOOP

--*--------------------------------------------------------------------*
--* Get payroll period from the date input value of the Research Leave
--* element.
--*--------------------------------------------------------------------*

   Select ptp.start_date
         ,ptp.end_date
   into   v_payroll_start_date
         ,v_payroll_end_date
   from   per_time_periods ptp
   where  emp.rl_date between ptp.start_date and ptp.end_date
   and    ptp.payroll_id = l_payroll;


   v_total_salary := 0;

--*--------------------------------------------------------------------*
--* The first time through the distribution loop gets the total salary 
--* that may have been distributed to multiple accounts.
--*--------------------------------------------------------------------*   
   For dist in salary_dist(emp.assignment_id) loop
      v_total_salary := v_total_salary + dist.distribution_amount;
   End Loop;

--*--------------------------------------------------------------------*
--* The 2nd time through the distribution loop gets the percentage of
--* the total to book to each account/object code.
--*--------------------------------------------------------------------* 
      
   For dist in salary_dist(emp.assignment_id) loop

      if dist.salary_name like '%9%' then
         v_rate := emp.salary / 1560;
      else
         v_rate := emp.salary / 2080;
      end if;

      v_amount := round(dist.distribution_amount / V_total_salary * v_rate * emp.hours,2);

      v_credit_acct_salary := nvl(dist.segment1,dist.account#)||dist.object; 


      earnings_record(out_file1, 'CO', emp.account#, dist.segment2, dist.object, emp.pid, dist.distribution_date,
                    emp.empl_name, v_amount, v_credit_acct_salary, emp.job_class);

      Update_research_leave_table('CO', emp.account#, dist.segment2, dist.object, emp.pid, dist.distribution_date,
                    emp.empl_name, v_amount, v_credit_acct_salary, emp.job_class);

--*--------------------------------------------------------------------*
--* Fringe
--*--------------------------------------------------------------------* 

      l_asg_acct := nvl(dist.segment1,dist.account#);
  
      get_fringe_account(l_asg_acct, emp.fr_acct, l_fr_account, l_chart_cd);

      v_amount := round(v_amount * dist.fringe_rate / 100,2);

      v_credit_acct_salary := l_fr_account||dist.fringe_obj; 

      earnings_record(out_file1, 'CO', emp.account#, dist.segment2, dist.fringe_obj, emp.pid, dist.distribution_date,
                    emp.empl_name, v_amount , v_credit_acct_salary, emp.job_class);

      Update_research_leave_table('CO', emp.account#, dist.segment2, dist.fringe_obj, emp.pid, dist.distribution_date,
                    emp.empl_name, v_amount , v_credit_acct_salary, emp.job_class);

   End Loop;

End Loop;

-- ***********************
-- * Write Net Record
-- ***********************
  If l_net <> 0 Then
    If l_net < 0 Then
       l_netpay_field := 'C';
       l_bank_field   := 'D';
    Else
       l_netpay_field := 'D';
       l_bank_field   := 'C';
    End if ;

-- schill - 432807
    utl_file.put_line(out_file1,
      l_fiscal_year||
      'CO'||
      rpad(nvl(substr(l_credit_acct_salary,1,7),' '),7,' ')||
      rpad(' ',5)||
      rpad(nvl(substr(l_credit_acct_salary,8,4),' '),4,' ')||
      '   '||
      'AC'||
      '  '||
      rpad(nvl(v_fiscal_period,' '),2,' ')|| 
      'PAYD'||
      l_origin_code||
      'PAYROLL-NETPAY'||
      '     '||
      rpad('PPE'||to_char(l_eff_date,'DD-MON-YYYY'),40,' ')||
      ltrim(to_char(abs(l_net),'09999999999999999.99'))||
      l_netpay_field||
      to_char(sysdate,'YYYY-MM-DD')||
      to_char(l_eff_date,'YYYY-MM-DD')||      -- org ref id
      rpad(' ',49));

      l_batch_count := l_batch_count + 1;
      l_batch_total_amount := l_batch_total_amount + abs(round(l_net,2));

    utl_file.put_line(out_file1,
      l_fiscal_year||
      'CO'||
      '0000500'||    -- Bank 05
      rpad(' ',5)||
      '1103'||
      '   '||
      'AC'||
      '  '||
      rpad(nvl(v_fiscal_period,' '),2,' ')|| 
      'PAYD'||
      l_origin_code||
      'PAYROLL-BANK05'||
      '     '||
      rpad('PPE'||to_char(l_eff_date,'DD-MON-YYYY'),40,' ')||
      ltrim(to_char(abs(l_net),'09999999999999999.99'))||
      l_bank_field||
      to_char(sysdate,'YYYY-MM-DD')||
      to_char(l_eff_date,'YYYY-MM-DD')||      -- org ref id
      rpad(' ',49));

      l_batch_count := l_batch_count + 1;
      l_batch_total_amount := l_batch_total_amount + abs(round(l_net,2));

  End if;

  l_total_summary_amount := l_total_summary_amount + abs(l_net);
  l_total_summary_amount := l_total_summary_amount + abs(l_net);

  utl_file.put_line(out_file2, 'c gl_entry_t ' || to_char(l_batch_count) ||';');
  utl_file.put_line(out_file2, 'S trn_ldgr_entr_amt ' || to_char(l_batch_total_amount) ||';');
  utl_file.put_line(out_file2, 'e 02;');

-- *************************************
-- *  Finish up and leave!
-- *************************************


select /*+ RULE */ payroll_name into l_payroll_name
    from pay_payrolls_f
   where payroll_id = l_payroll
     and l_end_date between effective_start_date and effective_end_date;

DBMS_OUTPUT.PUT_LINE('+----------------------------------------------------------+');
DBMS_OUTPUT.PUT_LINE('|                   HRMSS175 JOB SUMMARY                   |');
DBMS_OUTPUT.PUT_LINE('+----------------------------------------------------------+');
DBMS_OUTPUT.PUT_LINE('Parameters:');
DBMS_OUTPUT.PUT_LINE('..start date : '||to_char(l_start_date,'DD-MON-YYYY'));
DBMS_OUTPUT.PUT_LINE('..end date   : '||to_char(l_end_date,'DD-MON-YYYY'));
DBMS_OUTPUT.PUT_LINE('..payroll    : '||l_payroll_name);
DBMS_OUTPUT.PUT_LINE('Results:');
DBMS_OUTPUT.PUT_LINE('. Salary Transactions:');
DBMS_OUTPUT.PUT_LINE('.       Count        :'||lpad(to_char(ctl_earnings_cnt,'99999999'),15));
DBMS_OUTPUT.PUT_LINE('.       Amount       :'||lpad(to_char(ctl_sum_salary,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('. Fringe Transactions:');
DBMS_OUTPUT.PUT_LINE('.       Count        :'||lpad(to_char(ctl_earnings_sc, '99999999'),15));
DBMS_OUTPUT.PUT_LINE('.       Amount       :'||lpad(to_char(ctl_sum_fringe,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('. CPP Transactions   :');
DBMS_OUTPUT.PUT_LINE('.     Count          :'||lpad(to_char(ctl_cpp_cnt, '99999999'),15));
DBMS_OUTPUT.PUT_LINE('.     Amount         :'||lpad(to_char(ctl_sum_cpp,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('. Surcharge Transactions:');
DBMS_OUTPUT.PUT_LINE('.       Count        :'||lpad(to_char(ctl_surcharge_cnt,'99999999'),15));
DBMS_OUTPUT.PUT_LINE('.       Amount       :'||lpad(to_char(ctl_sum_surcharge,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('. Non-payroll Payments:');
DBMS_OUTPUT.PUT_LINE('.       Count        :'||lpad(to_char(ctl_cnt_non_payroll,'99999999'),15));
DBMS_OUTPUT.PUT_LINE('.       Amount       :'||lpad(to_char(ctl_sum_non_payroll,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('. Employee Deductions:');
DBMS_OUTPUT.PUT_LINE('.       Count        :'||lpad(to_char(ctl_deductions_cnt,'99999999'),15));
DBMS_OUTPUT.PUT_LINE('.       Amount       :'||lpad(to_char(ctl_sum_ee_deductions,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('. Cash Payments:');
DBMS_OUTPUT.PUT_LINE('.       Count        :'||lpad(to_char(ctl_cash_cnt,'99999999'),15));
DBMS_OUTPUT.PUT_LINE('.       Amount       :'||lpad(to_char(ctl_sum_cash_deductions,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('. Employer Deductions:');
DBMS_OUTPUT.PUT_LINE('.       Count        :'||lpad(to_char(ctl_deductions_sc,'99999999'),15));
DBMS_OUTPUT.PUT_LINE('.       Amount       :'||lpad(to_char(ctl_sum_er_deductions,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('.');
DBMS_OUTPUT.PUT_LINE('. Net Pay Amount     :'||lpad(to_char(l_net,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('.');
DBMS_OUTPUT.PUT_LINE('. Double Deduct Incl :'||lpad(to_char(l_double_deduction_inc,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('. Double Deduct Excl :'||lpad(to_char(l_double_deduction_exc,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('.');
DBMS_OUTPUT.PUT_LINE('. 3rd Party Check Amt:'||lpad(to_char(ctl_sum_3rd_party_checks,'$99,999,999.99'),15));
DBMS_OUTPUT.PUT_LINE('.');
DBMS_OUTPUT.PUT_LINE('. Payroll Batch Totals:');
DBMS_OUTPUT.PUT_LINE('.       Count        :'||lpad(to_char(l_batch_count,'99999999'),15));
DBMS_OUTPUT.PUT_LINE('.       Amount       :'||lpad(to_char(l_total_summary_amount,'$999,999,999.99'),16));
DBMS_OUTPUT.PUT_LINE('.');
DBMS_OUTPUT.PUT_LINE('. Gross - Net - EE Ded:'||lpad(to_char(ctl_sum_salary
                                                           - l_net
                                                           - ctl_sum_ee_deductions
                                                           - ctl_sum_cash_deductions,'$999,999,999.99'),16));
utl_file.fclose(out_file1);
utl_file.fclose(out_file2);

If (ctl_sum_salary - l_net - ctl_sum_ee_deductions - ctl_sum_cash_deductions) <> 0 Then
    DBMS_OUTPUT.PUT_LINE('###################################################');
    DBMS_OUTPUT.PUT_LINE('#####                                         #####');
    DBMS_OUTPUT.PUT_LINE('####                                           ####');
    DBMS_OUTPUT.PUT_LINE('###           KFS FILE IS OUT OF BALANCE        ###');
    DBMS_OUTPUT.PUT_LINE('####                                           ####');
    DBMS_OUTPUT.PUT_LINE('#####                                         #####');
    DBMS_OUTPUT.PUT_LINE('###################################################');
--    RAISE kfs_not_balanced;
End if;

commit;
    DBMS_OUTPUT.PUT_LINE('###################################################'   );
    DBMS_OUTPUT.PUT_LINE('#####                                              '   );
    DBMS_OUTPUT.PUT_LINE(' CSUH_REARCH_LEAVE_00 has been committed. If this  '   );  
    DBMS_OUTPUT.PUT_LINE(' job is rerun, remove '||l_RL_count||' transactions from this table. '); 
    DBMS_OUTPUT.PUT_LINE(' Remove those with '||l_todays_date||' in the first column and ');
    DBMS_OUTPUT.PUT_LINE(  to_char(l_research_seq)||' in the second column.  '   );                  
    DBMS_OUTPUT.PUT_LINE('#####                                              '   );                     
    DBMS_OUTPUT.PUT_LINE('###################################################'   );


DBMS_OUTPUT.PUT_LINE('.');
DBMS_OUTPUT.PUT_LINE
   ('**** End   of HRMSS175 ' ||to_char(sysdate,'MM/DD/YYYY HH24:MI:SS'));

Exception
  When null_params Then
     raise_application_error(-20000, '**** FATAL ERROR - PARAMATER MISSING! ****');
  When kfs_not_balanced Then
     Rollback;
     raise_application_error(-20000, '**** FATAL ERROR - KFS FILE IS OUT OF BALANCE! ****');
  When others then
     rollback;
     raise;
END;
/

--*  END   PROCESS TO CREATE KFS TRANSACTIONS

