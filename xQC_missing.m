function QC = xQC_missing(QC,sSubject,modality)


[~,txt,~] = xlsread('ParameterList.xlsx') ;
list = txt; 

ModIndex = strcmp(list(:,1), modality);

ModList = list(ModIndex,:);

for iPar = 1:length(ModList)
    
    QC.(sSubject).(ModList{iPar,1}).(ModList{iPar,2}).(ModList{iPar,3})= NaN;

end 



end 