# =============================================================================
#  build_orphan_dashboard.R  —  FULL dashboard (main-dashboard styling) to link
#  the REMAINING UNLINKED surveillance (orphan, not in MAIN) against orphan lab.
#  Tier 2 / Tier 3 / Tier 4 candidates are shown in SEPARATE review panels.
#  Lab sample histories are uid-keyed from the triangulated wide register.
#  Self-contained HTML; does NOT touch the MAIN linked data.
# =============================================================================
suppressWarnings(suppressMessages({library(jsonlite)}))
STD <- "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared/data"
TPL <- "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared/template/dashboard_template.html"
WIDEF<- "C:/Users/hp/Desktop/lab_data_triangulation/outputs/merged_eve_lab_wide.csv"
OUT <- "C:/Users/hp/Documents/data/DashShiny_EVE_Orphan/Orphan_Reconciliation_Dashboard.html"
rc<-function(p) read.csv(p,colClasses="character",check.names=FALSE,na.strings=character(0),encoding="UTF-8")
blank<-function(x){if(is.null(x))return(character(0));x[is.na(x)]<-"";trimws(x)}
pdate<-function(x){x<-trimws(blank(x));out<-as.Date(rep(NA_real_,length(x)),origin="1970-01-01")
  for(f in c("%Y-%m-%d","%d-%b-%Y","%d/%m/%Y")){m<-is.na(out)&nzchar(x);if(!any(m))break;out[m]<-suppressWarnings(as.Date(x[m],format=f))}
  ser<-is.na(out)&grepl("^[0-9]{5}$",x); if(any(ser)) out[ser]<-as.Date(as.numeric(x[ser]),origin="1899-12-30"); as.character(out)}

pot <- rc(file.path(STD,"orphan_potential_linkages.csv"))
osv <- rc(file.path(STD,"orphan_surveillance.csv"))
olab<- rc(file.path(STD,"orphan_lab.csv"))
W   <- rc(WIDEF); Wuid<-blank(W$patient_uid)
resCols<-grep("^final_result_evd_[0-9]+$",names(W),value=TRUE); resN<-as.integer(sub(".*_","",resCols)); resCols<-resCols[order(resN)]; resN<-sort(resN)

# uid-keyed sample history for each candidate lab patient
hist_of<-function(uid){ wi<-match(uid,Wuid); if(is.na(wi)) return(list())
  res<-blank(unlist(W[wi,resCols]))
  dv<-vapply(resN,function(k){c<-paste0("test_result_date_",k); if(c%in%names(W)) blank(W[[c]][wi]) else ""},character(1))
  dt<-pdate(dv); keep<-res %in% c("Positive","Negative","Inconnu","Not Tested","0");
  idx<-which(nzchar(res)); if(!length(idx)) return(list())
  o<-order(is.na(dt[idx]),dt[idx]); idx<-idx[o]
  lapply(idx,function(i) list(date=dt[i], result=res[i])) }
uids<-unique(blank(pot$lab_patient_uid)); HIST<-setNames(lapply(uids,hist_of),uids)

# extract the two CSS blocks from the main template so styling is identical
tpl<-readLines(TPL,warn=FALSE)
ss<-grep("<style>",tpl); ee<-grep("</style>",tpl)
css1<-paste(tpl[ss[1]:ee[ee>ss[1]][1]],collapse="\n")
css2<-if(length(ss)>=2) paste(tpl[ss[2]:ee[ee>ss[2]][1]],collapse="\n") else ""

POT<-toJSON(pot,dataframe="rows",auto_unbox=TRUE); HISTJ<-toJSON(HIST,auto_unbox=TRUE)
KPI<-toJSON(list(orph_surv=nrow(osv),orph_lab=nrow(olab),cand=nrow(pot),
  surv_with=length(unique(pot$surv_key)),no_cand=nrow(osv)-length(unique(pot$surv_key))),auto_unbox=TRUE)

html<-paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">
<title>Orphan Reconciliation Dashboard</title>
',css1,'
',css2,'
<style>
 body{background:var(--bg);} main{padding:18px 26px;max-width:1400px;margin:0 auto;}
 header.appbar{background:var(--navy);color:#fff;padding:14px 26px;box-shadow:var(--shadow);}
 header.appbar h1{margin:0;font-size:19px;} header.appbar .sub{opacity:.85;font-size:12.5px;margin-top:3px;}
 .tierhead{margin:26px 0 4px;font-size:16px;color:var(--navy);border-bottom:2px solid var(--line);padding-bottom:5px;}
 .tierhead .badge{vertical-align:middle;}
 .rq-pair .s{color:var(--ink);font-weight:600;} .rq-pair .l{color:var(--navy);}
 .grid{display:flex;gap:14px;flex-wrap:wrap;margin:8px 0 4px;}
 .card.kpi{flex:1;min-width:150px;padding:12px 16px;}
 tr.accepted{background:#EEF6E7;} tr.rejected{background:#FBEDED;opacity:.65;}
 .hist{font-size:11px;color:#444;line-height:1.5;} .hist b{color:var(--navy);}
</style></head><body>
<header class="appbar"><h1>Orphan Reconciliation Dashboard <span class="badge" style="background:var(--teal);color:#fff;">SEPARATE \u00b7 does not modify MAIN</span></h1>
<div class="sub">Linkage for the REMAINING UNLINKED surveillance (surveillance not in the confirmed MAIN linkage), matched only against orphan (non-linked) laboratory records. Tier 3 and Tier 4 candidates are reviewed in <b>separate panels</b>. Lab histories are uid-keyed from the 6-source triangulated register.</div></header>
<main>
 <div class="grid" id="kpis"></div>
 <div class="controls" style="margin:8px 0;"><input id="q" type="search" placeholder="Search surveillance / lab name\u2026" style="padding:7px 10px;border:1px solid var(--teal);border-radius:6px;min-width:300px;">
 <button class="rq-return" style="padding:6px 12px;border-radius:5px;cursor:pointer;" onclick="expo()">\u2193 Export decisions (CSV)</button>
 <span class="count" id="count"></span></div>
 <div id="panels"></div>
</main>
<script>
const POT=',POT,'; const HIST=',HISTJ,'; const KPI=',KPI,';
const LS="orphan_link_decisions"; let DEC={}; try{DEC=JSON.parse(localStorage.getItem(LS))||{}}catch(e){}
const esc=s=>String(s==null?"":s).replace(/[&<>"]/g,c=>({"&":"&amp;","<":"&lt;",">":"&gt;","\\"":"&quot;"}[c]));
const cid=p=>p.lab_patient_uid+"|"+p.surv_key;
const tnum=t=>{const m=/Tier ([0-9])/.exec(t||"");return m?+m[1]:9;};
function rpill(r){const t=String(r||"").toLowerCase();const c=t.indexOf("pos")==0?"pos":t.indexOf("neg")==0?"neg":"un";return \'<span class="pill \'+c+\'">\'+esc(r||"\u2014")+"</span>";}
function tpill(t){const n=tnum(t);const c=n<=2?"pos":n==3?"t3":"un";return \'<span class="pill \'+c+\'">\'+esc(String(t).replace(/ -.*/,""))+"</span>";}
function hist(uid){const h=HIST[uid]||[];if(!h.length)return \'<span class="muted">no sample history</span>\';
 return \'<div class="hist">\'+h.map((s,i)=>"<b>#"+(i+1)+"</b> "+esc(s.date||"?")+" "+rpill(s.result)).join(" \u00b7 ")+"</div>";}
function pairCell(p){return \'<div class="rq-pair"><span class="s">\'+esc(p.surv_name)+\'</span> <small class="muted">(\'+esc(p.surv_sex||"?")+", "+esc(p.surv_age||"?")+"y \u00b7 "+esc(p.surv_health_zone||"")+\')</small><br><span class="l">\'+esc(p.lab_name)+\'</span> <small class="muted">(\'+esc(p.lab_sex||"?")+", "+esc(p.lab_age||"?")+"y \u00b7 "+esc(p.lab_patient_uid||"")+")</small></div>";}
function decCell(id,d){ return d=="accepted"?\'<span class="pill neg">Accepted</span> <button class="rq-return" onclick="setd(\\\'\'+id+\'\\\',\\\'\\\')">\u21a9</button>\':
 d=="rejected"?\'<span class="pill un">Rejected</span> <button class="rq-return" onclick="setd(\\\'\'+id+\'\\\',\\\'\\\')">\u21a9</button>\':
 \'<div class="rq-actions"><button class="rq-accept" onclick="setd(\\\'\'+id+\'\\\',\\\'accepted\\\')">Accept</button><button class="rq-reject" onclick="setd(\\\'\'+id+\'\\\',\\\'rejected\\\')">Reject</button></div>\';}
function tierPanel(tier,rows,note,color){
 if(!rows.length) return "";
 const body=rows.map(p=>{const id=cid(p);const d=DEC[id]||"";
  return \'<tr class="\'+d+\'"><td>\'+tpill(p.candidate_tier)+"</td><td>"+pairCell(p)+"</td><td style=text-align:center>"+(p.sex_agreement=="True"?"\u2713":"\u2717")+"</td><td style=text-align:center>"+esc(p.age_diff)+"</td><td style=text-align:center>"+(p.age_within_5y=="True"?"\u2713":"\u2717")+"</td><td>"+rpill(p.lab_pcr)+"</td><td><small>"+esc(p.shared_tokens)+"</small>"+hist(p.lab_patient_uid)+"</td><td>"+decCell(id,d)+"</td></tr>";}).join("");
 return \'<h3 class="tierhead">\'+tier+\' <span class="badge" style="background:\'+color+\';color:#fff;">\'+rows.length+\' candidates</span></h3><div class="\'+(""+"")+\'" style="font-size:12.5px;color:var(--muted);margin-bottom:6px;">\'+note+\'</div><div class="table-scroll" style="max-height:460px;"><table class="tab-table"><thead><tr><th>Tier</th><th>Surveillance \u2194 candidate lab</th><th>Sex\u2713</th><th>Age \u0394</th><th>\u00b15y</th><th>Lab PCR</th><th>Evidence + sample history</th><th>Decision</th></tr></thead><tbody>\'+body+"</tbody></table></div>";}
function kpis(){document.getElementById("kpis").innerHTML=
 [["Orphan surveillance",KPI.orph_surv],["Orphan lab",KPI.orph_lab],["Surv with a candidate",KPI.surv_with],["Candidate rows",KPI.cand],["No candidate",KPI.no_cand]]
 .map(k=>\'<div class="card kpi"><div class="kpi-value">\'+k[1]+\'</div><div class="kpi-sub">\'+k[0]+"</div></div>").join("");}
function render(){const q=(document.getElementById("q").value||"").toLowerCase();
 const rows=POT.filter(p=>!q||((p.surv_name+" "+p.lab_name).toLowerCase().indexOf(q)>-1));
 const byT=n=>rows.filter(p=>tnum(p.candidate_tier)==n).sort((a,b)=>(+a.age_diff||99)-(+b.age_diff||99));
 document.getElementById("panels").innerHTML=
   tierPanel("Tier 2 \u2014 two or more shared name tokens",byT(2),"Higher confidence: at least two exact shared name tokens.","#548235")
  +tierPanel("Tier 3 \u2014 name + partial token + demographics",byT(3),"One exact token + a near/partial token, with sex agreement and age within \u00b15y.","#BF8F00")
  +tierPanel("Tier 4 \u2014 partial name parts (LOWER confidence \u2014 review carefully)",byT(4),"Loose partial name-part match only. Treat with caution; many will be rejected.","#C00000");
 const na=POT.filter(p=>DEC[cid(p)]=="accepted").length,nr=POT.filter(p=>DEC[cid(p)]=="rejected").length;
 document.getElementById("count").textContent=rows.length+" shown \u00b7 "+na+" accepted \u00b7 "+nr+" rejected";}
window.setd=function(id,v){if(v)DEC[id]=v;else delete DEC[id];localStorage.setItem(LS,JSON.stringify(DEC));render();};
window.expo=function(){const r=[["surv_key","surv_name","candidate_tier","lab_patient_uid","lab_name","lab_pcr","decision"]];
 POT.forEach(p=>r.push([p.surv_key,p.surv_name,p.candidate_tier,p.lab_patient_uid,p.lab_name,p.lab_pcr,DEC[cid(p)]||"pending"]));
 const csv=r.map(x=>x.map(c=>\'"\'+String(c==null?"":c).replace(/"/g,\'""\')+\'"\').join(",")).join("\\n");
 const a=document.createElement("a");a.href=URL.createObjectURL(new Blob([csv],{type:"text/csv"}));a.download="orphan_link_decisions.csv";a.click();};
document.getElementById("q").addEventListener("input",render); kpis(); render();
</script></body></html>')
writeLines(html,OUT,useBytes=TRUE)
cat("Orphan dashboard written:",OUT,"\n")
cat("candidates:",nrow(pot)," | Tier2:",sum(grepl("Tier 2",pot$candidate_tier))," Tier3:",sum(grepl("Tier 3",pot$candidate_tier))," Tier4:",sum(grepl("Tier 4",pot$candidate_tier)),"\n")
