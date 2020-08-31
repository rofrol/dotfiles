#! /bin/bash
### BEGIN INIT INFO
# Provides:          dns-sync
# Required-Start:
# Required-Stop:
# Default-Start:     S
# Default-Stop:
# Short-Description: Synchronizes /etc/resolv.conf in WLS with Windows DNS - Matthias Brooks
### END INIT INFO

PATH=/sbin:/bin
PS=/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe

. /lib/init/vars.sh
. /lib/lsb/init-functions

do_start () {
        while true
        do
          #Retrieve nameservers from via Powershell
          TEMPFILE=$(mktemp)
          $PS -Command "Get-DnsClientServerAddress -AddressFamily IPv4 | Select-Object -ExpandProperty ServerAddresses" > $TEMPFILE
          /usr/bin/awk '!x[$0]++' $TEMPFILE > $TEMPFILE.2
          IFS=$'\r\n' GLOBIGNORE='*' command eval  'UNIQUE_NAMESERVERS=($(cat $TEMPFILE.2))'
          rm -f $TEMPFILE $TEMPFILE.2
          
          #Retrive search domains via powershell
          IFS=$'\r\n' GLOBIGNORE='*' command eval  'SEARCH_DOMAIN=($($PS -Command "Get-DnsClientGlobalSetting | Select-Object -ExpandProperty SuffixSearchList"))'
          UNIQUE_SEARCH_DOMAIN=($(/usr/bin/tr ' ' '\n' <<< "${SEARCH_DOMAIN[@]}" | /usr/bin/sort -u | /usr/bin/tr '\n' ' '))

          
          #Modify /etc/resolv.conf
          touch /etc/resolv.conf
          sed -i '/nameserver/d' /etc/resolv.conf > /dev/null  2>&1 || true
          sed -i '/search/d' /etc/resolv.conf > /dev/null  2>&1 || true

          for i in "${UNIQUE_NAMESERVERS[@]}"
          do
            echo "$(date --rfc-3339=seconds) nameserver ${i}" >> /var/log/dns-sync.log
            # Do not echo server starting with 192. because then internal github is not reachable.
            # Unless it is the only server, which means we are not on vpn.
            if [[ ! ${i} == 192.* || ${#UNIQUE_NAMESERVERS[@]} == 1 ]]; then
              echo "$(date --rfc-3339=seconds) adding nameserver ${i}" >> /var/log/dns-sync.log
              echo "nameserver ${i}" >> /etc/resolv.conf
            fi 
          done
          if [ ${#UNIQUE_SEARCH_DOMAIN[@]} -ne 0 ]; then
            echo "search ${UNIQUE_SEARCH_DOMAIN[@]}" >> /etc/resolv.conf
          fi
          sleep 15
        done
}

do_status () {
        PID=$(cat /var/run/dns-sync.pid 2>/dev/null)
        if [ "$PID" == "" ]; then
          echo "dns-sync is not running"
          return
        fi
        
        if ps -p $PID > /dev/null
        then
           echo "dns-sync is running"
        else
           echo "dns-sync is not running"
        fi
}

case "$1" in
  start|"")
        kill $(cat /var/run/dns-sync.pid >/dev/null 2>&1) > /dev/null  2>&1 || true
        do_start &
        DO_SYNC_PID=$!
        echo "${DO_SYNC_PID}" > /var/run/dns-sync.pid
        ;;
  restart|reload|force-reload)
        echo "Error: argument '$1' not supported" >&2
        exit 3
        ;;
  stop)
        PID=$(cat /var/run/dns-sync.pid)
        if ps -p $PID > /dev/null
        then
           kill -9 $(cat /var/run/dns-sync.pid)
           echo "dns-sync stopped"
        else
           echo "dns-sync is not running"
        fi
        ;;
  status)
        do_status
        exit $?
        ;;
  *)
        echo "Usage: dns-sync.sh [start|stop]" >&2
        exit 3
        ;;
esac
