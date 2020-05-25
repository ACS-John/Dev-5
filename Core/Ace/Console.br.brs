! Replace S:\Core\Ace\Console.br
! function to hide/show the BR Console
!
def library fnConsole(;on_off)
	option retain 
	if on_off<>windowstate then  !   : pr "WindowState remains the same.
		if ~on_off then 
			execute 'Config GUI On'
		else if on_off then 
			execute 'Config GUI off'
		end if 
		windowstate=on_off
	end if
fnend 
