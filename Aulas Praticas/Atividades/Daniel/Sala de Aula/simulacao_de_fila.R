fila <- function(Time, l1, l2, t = 0, tc = rexp(1, l1), ts = Inf, n = 0, Nc = 0, Ns = 0, C = NULL, S = NULL, Tp = 0){
  if(min(tc,ts) > Time & n == 0) return(list(Time = Time, Chegadas = C, Saidas = S, Ultimo.tempo = max(0, t-Time)))
  if(min(tc,ts) > Time & n > 0){
    t <- ts
    n <- n - 1
    Ns <- Ns + 1
    if(n > 0) ts <- t + rexp(1,l2)
    S[Ns] <- t
    return(fila(Time, l1, l2, t, tc, ts, n, Nc, Ns, C, S, Tp))
  }
  if(ts < tc & ts < Time){
    t <- ts ; n <- n - 1
    Ns <- Ns + 1
    if(n == 0) ts <- Inf
    ts <- t + rexp(1, l2)
    S[Ns] <- t
    return(fila(Time, l1, l2, t, tc, ts, n, Nc, Ns, C, S, Tp))
  }
  if(tc < ts & tc < Time){
    t <- tc ; Nc <- Nc + 1
    n <- n + 1 ;
    tc <- tc + rexp(1, l1)
    if(n == 1){
      ts <- t + rexp(1, l2)
    }
    C[Nc] <- t
    return(fila(Time, l1, l2, t, tc, ts, n, Nc, Ns, C, S, Tp))
  }
} 
fila(10,1,2)
