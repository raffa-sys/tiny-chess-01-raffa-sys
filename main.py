import json,os,urllib.request as R
M=[0,100,320,330,500,900,0]
n=(-31,-33,-14,-18,14,18,31,33);d=(-17,-15,15,17);r=(-16,-1,1,16);g=d+r
PD=(n,d,r,g,g)
I=1000000000;F=10000
Z={4:12,0:13,7:14,116:3,112:7,119:11}
CT=[6-abs(q%8-3)-abs(q//16-3)for q in range(128)]
WP=[0,0,6,5,3,2,0]
B=[0]*128;S=1;E=-1;H=0;K=[4,116];U=[];T={};KL=[0]*16;N=[0];L=100000;RP=[0]*64
V=os.environ['MAKEFUN_ENV_URL']
def at(q,w):
 b=B;s=1 if w else -1
 for o,v,l in(((-15*s,-17*s),1,0),(n,2,0),(g,6,0),(d,3,1),(r,4,1)):
  for t in o:
   u=q+t
   while not u&0x88:
    p=b[u]
    if p:
     if p==v*s or(l and p==5*s):return 1
     break
    if not l:break
    u+=t
 return 0
def ev():
 b=B;m=M;c=CT;w=WP
 s=0
 for q,p in enumerate(b):
  if p:
   a=abs(p);v=m[a]+c[q]*w[a]
   if a==1:v+=((q>>4)-1 if p>0 else 6-(q>>4))*8
   s+=v if p>0 else -v
 return s*S
def pm(f,t):
 return [f|t<<7|p<<14 for p in(5,4,3,2)]if(t>>4)==7*(S>0)else[f|t<<7]
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
    if(f>>4)==1+5*(S<0)and b[f+2*z]==0:ms.append(f|(f+2*z)<<7)
   for w in(-1,1):
    t=f+z+w
    if t&0x88:continue
    if b[t]*S<0 or t==E:ms+=pm(f,t)
  else:
   o=PD[a-2];l=9 if 2<a<6 else 1
   for z in o:
    t=f+z
    for _ in range(l):
     if t&0x88:break
     if b[t]:
      if b[t]*S<0:ms.append(f|t<<7)
      break
     if not co:ms.append(f|t<<7)
     t+=z
   if a==6 and not co:
    for k in(1,-1):
     if C>>(2*(S<0)+(k<0))&1 and b[f+k]==0==b[f+2*k]and(k>0 or b[f+3*k]==0)and not any(at(f+i*k,S<0)for i in(0,1,2)):ms.append(f|(f+2*k)<<7)
 return ms
def mk(m):
 global S,C,E,H
 f=m&127;t=m>>7&127;pr=m>>14;p=B[f];a=abs(p);cap=B[t];cs=t
 if a==1 and t==E and cap==0:cs=t-16*S;cap=B[cs];B[cs]=0
 U.append((cap,cs,C,E,H))
 B[t]=pr*S if pr else p;B[f]=0
 if a==6 and abs(t-f)==2:
  rf,rt=(t+1,t-1)if t>f else(t-2,t+1);B[rt]=B[rf];B[rf]=0
 if a==6:K[S<0]=t
 C&=Z.get(f,15)&Z.get(t,15)
 E=(f+t)//2 if a==1 and abs(t-f)==32 else -1
 H=0 if a==1 or cap else H+1;S=-S
def un(m):
 global S,C,E,H
 f=m&127;t=m>>7&127;pr=m>>14;cap,cs,C,E,H=U.pop()
 S=-S;a=abs(B[t])
 if a==6 and abs(t-f)==2:
  rf,rt=(t+1,t-1)if t>f else(t-2,t+1);B[rf]=B[rt];B[rt]=0
 if a==6:K[S<0]=f
 B[f]=S if pr else B[t];B[t]=0
 if cap:B[cs]=cap
def sc(d,a,b,pl):
 N[0]+=1
 if N[0]>L:return a
 if H>=100:return 0
 t=0;o=a
 if d<=0:
  v=ev();a=max(a,v);t=v
  if a>=b:return b
  if pl>8:return a
  ms=gn(1)
 else:
  ms=gn()
  k=(tuple(B),S,C,E);e=T.get(k)
  if k in RP[:pl]:return 0
  RP[pl]=k
  if e and e[0]is not None and e[1]>=d:return e[0]
  t=e[2]if e else 0
 ms.sort(key=lambda m:(m==t or m==KL[pl],M[abs(B[m>>7&127])]),reverse=True)
 lc=0;bb=0
 for m in ms:
  if d<=0 and t+M[abs(B[m>>7&127])or 1]+200<a:continue
  s=S;mk(m)
  if at(K[s<0],s<0):un(m);continue
  lc+=1;v=-sc(d-1,-b,-a,pl+1);un(m)
  if v>a:a=v;bb=m
  if a>=b:KL[pl]=m;break
 if d>0 and bb:T[k]=(a if o<a<b and N[0]<=L else None,d,bb)
 if lc==0 and d>0:return pl-F if at(K[S<0],S<0) else 0
 return a
def rt(D):
 T.clear();N[0]=0;bb=0;RP[0]=(tuple(B),S,C,E)
 for d in range(1,D+1):
  v2=-I;b2=0
  for m in gn():
   s=S;mk(m)
   if not at(K[s<0],s<0):
    v=-sc(d-1,-I,I,1)
    if v>v2:v2=v;b2=m
   un(m)
  if N[0]>L:break
  bb=b2
 return bb
def sq(s):return ord(s[0])-97+(ord(s[1])-49)*16
def nm(f):return chr(97+(f&7))+chr(49+(f>>4))
def sf(f):
 global B,S,C,E,H;B=[0]*128;p=f.split();q=112;U.clear()
 for c in p[0]:
  if c=='/':q-=24
  elif c.isdigit():q+=int(c)
  else:
   v='PNBRQK'.index(c.upper())+1;B[q]=-v if c.islower() else v
   if v==6:K[c=='k']=q
   q+=1
 S=1 if p[1]=='w' else -1
 C=sum(1<<'KQkq'.find(c)for c in p[2]if c in'KQkq')
 E=sq(p[3])if p[3]!='-'else -1
 H=int(p[4])
def P(p,b):
 return json.load(R.urlopen(V+p,json.dumps(b).encode()))
if __name__=='__main__':
 while 1:
  G=P('/game',{})
  if G['status']=='done':break
  while G['status']=='your_move':
   sf(G['fen']);m=rt(4)
   G=P('/move',{'game':G['game'],'move':nm(m&127)+nm(m>>7&127)+('nbrq'[(m>>14)-2]if m>>14 else'')})
