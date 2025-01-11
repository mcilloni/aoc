#!/usr/bin/env pwsh

# AoC 2024 Day 13

function Get-Solutions {
    Param (
        [Parameter(Mandatory = $true, ValueFromPipeline = $true)]
        [hashtable[]]
        $Data
    )

    Begin {}

    Process {
        ForEach-Object -InputObject $Data {
            $params = @{
                x1 = $_.A.X
                y1 = $_.A.Y
                x2 = $_.B.X
                y2 = $_.B.Y
                x  = $_.Prize.X
                y  = $_.Prize.Y
            }
            
            Invoke-Solver $params
        }
    }
}

function Import-Button([string] $line) {
    if ("$line" -Match "Button\s*(?<id>[A-Z]):\s*X\+(?<X>\d+),\s*Y\+(?<Y>\d+)") {
        @{
            Id = $Matches.id
            X  = [double] $Matches.X
            Y  = [double] $Matches.Y
        }
    }
    else {
        throw "error: invalid button format: $line"
    }   
}

function Import-Prize([string] $line) {
    if ($line -Match "Prize\s*:\s*X=(?<X>\d+),\s*Y=(?<Y>\d+)") {
        @{
            X = [double] $Matches.X
            Y = [double] $Matches.Y
        }
    }
    else {
        throw "error: invalid prize format: $line"
    }
}

function Invoke-Solver($params) {
    # unpack parameters from the hashtable $params to local variables
    $params.GetEnumerator() | ForEach-Object {
        Set-Variable -Name $_.Key -Value $_.Value
    }

    # solved using Cramer's rule or by hand, it matters not
    $a = ($x * $y2 - $x2 * $y) / ($x1 * $y2 - $x2 * $y1)
    $b = ($x1 * $y - $x * $y1) / ($x1 * $y2 - $x2 * $y1)

    @{
        Na = $a
        Nb = $b
    }
}

function Measure-Solution($solution) {
    Set-Variable -Name CostA -Value 3 -Option Constant
    Set-Variable -Name CostB -Value 1 -Option Constant

    $solution.Na * $CostA + $solution.Nb * $CostB
}

function Read-Input($filePath) {
    Get-Content -ReadCount 4 $filePath | ForEach-Object {
        $a, $b, $prize, $ignored = $_
        
        @{
            A     = Import-Button $a
            B     = Import-Button $b
            Prize = Import-Prize $prize
        }
    }
}

function Test-Integer([double] $n) {
    [math]::Floor($n) -eq $n
}

function Test-Solution($solution) {
    if (($solution.Na -lt 0) -or ($solution.Nb -lt 0)) {
        return $false
    }
    
    return Test-Integer $solution.Na -and Test-Integer $solution.Nb
}

function Update-Prize($params, $offset) {
    @{
        A     = $params.A
        B     = $params.B
        Prize = @{
            X = $params.Prize.X + $offset
            Y = $params.Prize.Y + $offset
        }
    }
}

function main($filePath) {
    $inputs = Read-Input $filePath;
    
    $part1 = $inputs | Get-Solutions | Where-Object {
        Test-Solution $_
    } | ForEach-Object {
        Measure-Solution $_
    } | Measure-Object -Sum | Select-Object -ExpandProperty Sum

    Write-Output "part1: $part1"

    $part2 = $inputs | ForEach-Object {
        Update-Prize $_ 10000000000000
    } | Get-Solutions | Where-Object {
        Test-Solution $_
    } | ForEach-Object {
        Measure-Solution $_
    } | Measure-Object -Sum | Select-Object -ExpandProperty Sum

    Write-Output "part2: $part2"
}

if ($args.Length -ne 1) {
    Write-Error "Usage: $($MyInvocation.MyCommand) FILE"
    
    exit 2
}

main $args[0]
