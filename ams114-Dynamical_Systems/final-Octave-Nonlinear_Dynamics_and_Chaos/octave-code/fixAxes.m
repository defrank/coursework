function fixAxes
%---------------------------------------
%// Kludge to fix scaling of all figures
%// until GNU or I can find real fix.
%// Octave3.2.3 computes the scaling wrong
%// for this mac, such that the title
%// and xlabel are not displayed.
%---------------------------------------
s = get(0,'showhiddenhandles');
set(0,'showhiddenhandles','on');
newpos = [0.13 0.135 0.775 0.75];        %// default is [0.13 0.11 0.775 0.815]
figs = get(0,'children');
if (~isempty(figs))
   for k=1:length(figs)
            cax = get(figs(k),'currentaxes');
pos = get(cax,'position');
if ~(pos(1) == newpos(1) && ...
     pos(2) == newpos(2) && ...
     pos(3) == newpos(3) && ...
     pos(4) == newpos(4))
set(cax,'position',newpos);
set(0,'currentfigure',figs(k));
drawnow();
        endif
            endfor
        endif
        set(0,'showhiddenhandles',s);
%---------------------------------------
endfunction
%---------------------------------------

