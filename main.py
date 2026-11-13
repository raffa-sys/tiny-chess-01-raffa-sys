import sys
M=[0,100,320,330,500,900,0]
n=(-31,-33,-14,-18,14,18,31,33);g=(-17,-16,-15,-1,1,15,16,17);d=(-17,-15,15,17);r=(-16,-1,1,16)
I=10**9;F=100000;W='abcdefgh';R='12345678'
Z={4:12,0:13,7:14,116:3,112:7,119:11}
B=[0]*128;S=1;E=-1;H=0;K=[4,116]
A='rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'
def at(q,w):
 b=B;s=1 if w else -1
 for t in(-15,-17)if w else(15,17):
  if not(q+t)&0x88 and b[q+t]==s:return 1
 for o,v in((n,2),(g,6)):
  for t in o:
   if not(q+t)&0x88 and b[q+t]==v*s:return 1
 for o,v in((d,3),(r,4)):
  for t in o:
   u=q+t
   while not u&0x88:
    p=b[u]
    if p:
     if p==v*s or p==5*s:return 1
     break
    u+=t
 return 0
def ev():
 s=sum(M[abs(p)]*(1 if p>0 else -1)for p in B if p)+30*(B.count(3)//2-B.count(-3)//2)
 return s if S>0 else -s
def pm(f,t):
 return [f|t<<7|p<<14 for p in(5,4,3,2)]if(t>>4)==(7 if S>0 else 0)else[f|t<<7]
def gn(co=0):
 b=B;ms=[]
 for f in range(128):
  p=b[f]
  if not p or p*S<=0:continue
  a=abs(p)
  if a==1:
   z=16*S;t=f+z
   if not co and b[t]==0:
    ms+=pm(f,t)
    if(f>>4)==(1 if S>0 else 6)and b[f+2*z]==0:ms.append(f|(f+2*z)<<7)
   for w in(z-1,z+1):
    t=f+w
    if t&0x88:continue
    if b[t]*S<0 or t==E:ms+=pm(f,t)
  elif a==2 or a==6:
   for z in(n if a==2 else g):
    t=f+z
    if t&0x88:continue
    if(b[t]==0 and not co)or b[t]*S<0:ms.append(f|t<<7)
   if a==6 and not co:
    for kt,bt in((6,1),(2,2))if f==4 else((118,4),(114,8))if f==116 else():
     if C&bt and all(b[s]==0 for s in((f+1,kt)if kt>f else(f-1,f-2,f-3)))and not any(at(s,S<0)for s in((f,kt-1,kt)if kt>f else(f,kt+1,kt))):ms.append(f|kt<<7)
  else:
   for z in(d if a==3 else r if a==4 else d+r):
    t=f+z
    while not t&0x88:
     if b[t]==0:
      if not co:ms.append(f|t<<7)
     else:
      if b[t]*S<0:ms.append(f|t<<7)
      break
     t+=z
 return ms
def mk(m):
 global S,C,E,H;b=B;f=m&127;t=(m>>7)&127;pr=m>>14;p=b[f];a=abs(p);cap=b[t]
 if a==1 and t==E and cap==0:e=t-16*S;cap=b[e];b[e]=0
 b[t]=(pr if S>0 else -pr)if pr else p;b[f]=0;af=abs(t-f)
 if a==6 and af==2:
  rf,rt=(t+1,t-1)if t%16==6 else(t-2,t+1);b[rt]=b[rf];b[rf]=0
 if a==6:K[0 if S>0 else 1]=t
 C&=Z.get(f,15)&Z.get(t,15)
 if a==1 and af==32:E=(f+t)//2
 else:E=-1
 H=0 if a==1 or cap else H+1;S=-S
def sv():
 return B[:],S,C,E,H,K[:]
def rs(o):
 global S,C,E,H;B[:],S,C,E,H,K[:]=o
def sc(d,a,be,ply):
 if H>=100:return 0
 if d<=0:return ev()
 ms=gn();ms.sort(key=lambda m:M[abs(B[(m>>7)&127])],reverse=True);bs=-I;lc=0
 for m in ms:
  s=S;o=sv();mk(m)
  if at(K[0 if s>0 else 1],s<0):rs(o);continue
  lc+=1;v=-sc(d-1,-be,-a,ply+1);rs(o)
  if v>bs:bs=v
  if v>a:a=v
  if a>=be:break
 if lc==0:return -F+ply if at(K[0 if S>0 else 1],S<0) else 0
 return bs
def rt(d):
 ms=[]
 for m in gn():
  s=S;o=sv();mk(m)
  if not at(K[0 if s>0 else 1],s<0):ms.append(m)
  rs(o)
 bv=-I;bb=0
 for m in ms:
  o=sv();mk(m);v=-sc(d-1,-I,I,1);rs(o)
  if v>bv:bv=v;bb=m
 return bb
def sf(f):
 global B,S,C,E,H;B=[0]*128;p=f.split();q=112
 for c in p[0]:
  if c=='/':q-=24
  elif c.isdigit():q+=int(c)
  else:
   v='PNBRQK'.index(c.upper())+1
   if c.islower():v=-v
   B[q]=v
   if abs(v)==6:K[0 if v>0 else 1]=q
   q+=1
 S=1 if p[1]=='w' else -1;C=0
 if len(p)>2:
  for i in range(4):
   if 'KQkq'[i] in p[2]:C|=1<<i
 E=W.index(p[3][0])+(int(p[3][1])-1)*16 if len(p)>3 and p[3]!='-' else -1
 H=int(p[4])if len(p)>4 else 0
def loop():
 sf(A)
 for line in sys.stdin:
  w=line.split()
  if not w:continue
  c=w[0]
  if c=='uci':print('uciok',flush=True)
  elif c=='isready':print('readyok',flush=True)
  elif c=='quit':break
  elif c=='position':
   i=1
   if w[1]=='startpos':sf(A);i=2
   elif w[1]=='fen':sf(' '.join(w[2:8]));i=8
   if i<len(w) and w[i]=='moves':
    for mv in w[i+1:]:mk(W.index(mv[0])+R.index(mv[1])*16|(W.index(mv[2])+R.index(mv[3])*16)<<7|(('nbrq'.index(mv[4])+2)if len(mv)>4 else 0)<<14)
  elif c=='go':
   d=4
   if 'depth' in w:d=min(int(w[w.index('depth')+1]),6)
   m=rt(d);f=m&127;t=(m>>7)&127;pr=m>>14
   print('bestmove',W[f&7]+R[f>>4]+W[t&7]+R[t>>4]+('nbrq'[pr-2]if pr else'')if m else 'bestmove 0000',flush=True)
if __name__=='__main__':loop()
