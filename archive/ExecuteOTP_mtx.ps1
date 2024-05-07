# https://social.technet.microsoft.com/wiki/contents/articles/7703.powershell-running-executables.aspx

$CMD = 'C:\jython2.7.0\bin\jython.exe'
$arg1 = '-J-XX:-UseGCOverheadLimit'
$arg2 = '-J-Xmx24G'
$arg3 = '-Dpython.path=otp-1.3.0-shaded.jar'
$arg4 = 'python_script_loopHM_parallel.py'

& $CMD $arg1 $arg2 $arg3 $arg4 

# 2> $null