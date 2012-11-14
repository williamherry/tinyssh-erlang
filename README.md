erlang version of tinyssh

Install:

- install erlang
- download `tinyssh` and `tinyssh.erl`
- make sure `tinyssh` and `tinyssh.erl` in the same directory
- make sure `tinyssh` has `x` permission
- link `tinyssh` to the directory that your `$PATH` contains

Usage:

    tinyssh run 'ls -l /tmp' -l ips.txt -u username -p 22 -k password
    tinyssh upload|download from_file to_file -l ips.txt -u username -p 22 -k password

Node: ips.txt contains the ip list of your servers
