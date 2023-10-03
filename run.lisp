(load "usufslc.asd")
(ql:quickload 'usufslc)
(usufslc:start)
(bt:join-thread
  (find-if (lambda (th)
             (search "woo" (bt:thread-name th)))
           (bt:all-threads)))
