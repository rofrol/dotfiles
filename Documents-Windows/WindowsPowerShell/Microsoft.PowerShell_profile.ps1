<#

https://superuser.com/questions/106360/how-to-enable-execution-of-powershell-scripts

and then:

``` powershell
PS > new-item -type file -path $profile -force


    Directory: C:\Users\rofrol\Documents\WindowsPowerShell


Mode                LastWriteTime         Length Name
----                -------------         ------ ----
-a----       2018-09-04     20:09              0 Microsoft.PowerShell_profile.ps1
```

https://stackoverflow.com/questions/8997316/powershell-profile-is-pointing-to-a-path-that-i-cant-find-and-setting-permane/8997378#8997378

#>

function Stop-ProcessByPort( [ValidateNotNullOrEmpty()] [int]$port)
{
  Stop-Process -Id (Get-NetTCPConnection -LocalPort $port).OwningProcess -Force
}

# set alias killp
sal killp Stop-ProcessByPort

<#>

Restart shell and use:

```powershell
killp 4200
```

- https://stackoverflow.com/questions/39632667/how-to-kill-the-process-currently-using-a-port-on-localhost-in-windows/47717839#47717839
- https://stackoverflow.com/questions/6204003/kill-a-process-by-looking-up-the-port-being-used-by-it-from-a-bat/40388270#40388270
- https://superuser.com/questions/455280/windows-fastest-way-of-killing-a-process-running-on-a-specific-port
- https://github.com/majkinetor/posh/blob/master/MM_Network/Stop-ProcessByPort.ps1
#>

function Add-ToPath {
    param(
        [string]$Dir
    )

    # if ( !(Test-Path $Dir) ) {
    #     Write-warning "Supplied directory was not found!"
    #     return
    # }
    $PATH = [Environment]::GetEnvironmentVariable("PATH", "User")
    # if ( $PATH -notlike "*" + $Dir + "*" ) {
    [Environment]::SetEnvironmentVariable("PATH", "$PATH;$Dir", "User")
    # }
}

sal pathadd Add-ToPath

<#
Usage:

`pathadd '%USERPROFILE%\.cargo\bin'`

- https://stackoverflow.com/questions/9546324/adding-a-directory-to-the-path-environment-variable-in-windows/29109007#29109007
#>