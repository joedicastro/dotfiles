Also would be necessary to create these executable bash files to call them
instead of the original binary files

* /usr/local/bin/jailfox
~~~
firejail /usr/bin/firefox "$@"
~~~

* /usr/local/bin/chromium
~~~
firejail /usr/bin/chromium "$@"
~~~

* /usr/local/bin/zathura
~~~
firejail /usr/bin/zathura "$@"
~~~

* /usr/local/bin/transmission-daemon
~~~
firejail /usr/bin/transmission-daemon
~~~
