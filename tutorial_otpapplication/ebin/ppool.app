{application, ppool,
 [{vsn, "1.0.0"},
  {description, "ppool application following the book"},
  {modules, [ppool, ppool_serv, ppool_sup, ppool_supersup, ppool_worker_sup]},
  {registered, [ppool]},
  {mod, {ppool, []}}
]}.
