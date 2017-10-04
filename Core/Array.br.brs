28200 def library fnarray_item_insert$(mat array$, insert_item$*1024, insert_item_number)
28300   fnarray_item_insert$=fn_array_item_insert$(mat array$, insert_item$, insert_item_number)
28400 fnend 
28500 def fn_array_item_insert$(mat array$, insert_item$*1024, insert_item_number)
28600   array_item_count=udim(mat array$)
28700   if insert_item_number>array_item_count then 
28800     mat array$(insert_item_number)
28900     array$(insert_item_number)=insert_item$
29000   else 
29100     array_item_count+=1
29200     mat array$(array_item_count)
29300     mat array$(insert_item_number+1:array_item_count)=array$(insert_item_number:array_item_count-1)
29400     array$(insert_item_number)=insert_item$
29500   end if  ! Insert_Item_Number>Array_Item_Count   /   else 
29600 fnend 
29700 def library fnarray_item_insert(mat array, insert_item, insert_item_number)
29800   fnarray_item_insert=fn_array_item_insert(mat array, insert_item, insert_item_number)
29900 fnend  ! fnARRAY_ITEM_INSERT
30000 def fn_array_item_insert(mat array, insert_item, insert_item_number)
30100   array_item_count=udim(mat array)
30200   if insert_item_number>array_item_count then 
30300     mat array(insert_item_number)
30400     array(insert_item_number)=insert_item
30500   else 
30600     array_item_count+=1
30700     mat array(array_item_count)
30800     mat array(insert_item_number+1:array_item_count)=array(insert_item_number:array_item_count-1)
30900     array(insert_item_number)=insert_item
31000   end if  ! Insert_Item_Number>Array_Item_Count   /   else 
31100 fnend 
31200 def library fnsrch_case_insensitive(mat srch_array$,srch_for$*80; srch_start_ele)
31300   ! if ~setup the let fn_setup
31400   fnsrch_case_insensitive=fn_srch_case_insensitive(mat srch_array$,srch_for$, srch_start_ele)
31500 fnend  ! fnsrch_case_insensitive
31600 def fn_srch_case_insensitive(mat srch_array$,srch_for$*80; srch_start_ele)
31700   srch_array_count=udim(mat srch_array$)
31800   srch_return=0
31900   do 
32000     srch_found=srch(mat srch_array$,'^'&srch_for$,srch_start_ele)
32100     if srch_found>0 and lwrc$(srch_for$)=lwrc$(srch_array$(srch_found)) then 
32200       srch_return=srch_found
32300     else if srch_found>0 then 
32400       srch_start_ele=srch_found+1
32500     else if srch_found<=0 then ! it's not there, anywhere - get outta here.
32600       srch_start_ele=srch_array_count+1
32700     end if 
32800   loop until srch_start_ele>srch_array_count or srch_return
32900   fn_srch_case_insensitive=srch_return
33000 fnend  ! fn_srch_case_insensitive
33100 def library fnAddOneN(mat add_to, one; skip_zeros, skip_dupes)
33200   fnAddOneN=fn_addOneN(mat add_to, one, skip_zeros, skip_dupes)
33300 fnend 
33400 def fn_addOneN(mat add_to, one; skip_zeros, skip_dupes)
33500   ! must dim an array to 0 before you can add a first item
33600   !    Mat Add_To - the array to add One item to
33700   !    One - the One item to add to Mat Add_To
33800   !    skip_Zeros - if =1 than only add One if One<>0
33900   !    skip_dupes - if =1 than only add One if One is not yet in Mat Add_To
34000   !    This function returns the number of items in the array after any add
34100   if skip_zeros=0 or (skip_zeros and one<>0) then 
34200     if skip_dupes=0 or (skip_dupes and srch(mat add_to,one)=-1) then 
34300       add_to_udim=udim(add_to) : mat add_to(add_to_udim+1) : add_to(add_to_udim+1)=one
34400     end if 
34500   end if 
34600   fn_addOneN=udim(mat add_to)
34700 fnend 
34800 def library fnAddOneC(mat add_to$, one$*2048; skip_blanks, skip_dupes)
34900   fnAddOneC=fn_addOneC(mat add_to$, one$, skip_blanks, skip_dupes)
35000 fnend 
35100 def fn_addOneC(mat add_to$, one$*2048; skip_blanks, skip_dupes)
35200   ! must dim an array to 0 before you can add a first item
35300   !    Mat Add_To$ - the array to add One$ item to
35400   !    One$ - the One$ item to add to Mat Add_To$
35500   !    skip_Blanks - if =1 than only add One$ if Trim$(One$)<>""
35600   !    skip_dupes - if =1 than only add One$ if One$ is not yet in Mat Add_To$
35700   !    This function returns the number of items in the array after any add
35800   if skip_blanks=0 or (skip_blanks and trim$(one$)<>"") then 
35900     if skip_dupes=0 or (skip_dupes and srch(mat add_to$,one$)=-1) then 
36000       add_to_udim=udim(mat add_to$) : mat add_to$(add_to_udim+1) : add_to$(add_to_udim+1)=one$
36100     end if 
36200   end if 
36300   fn_addOneC=udim(mat add_to$)
36400 fnend 