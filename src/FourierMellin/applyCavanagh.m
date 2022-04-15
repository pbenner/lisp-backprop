n = 64;

system('rm -f raw.txt || true');
system('rm -f features.txt || true');

%C = ['A','B'];
C = ['A','B','C','D','E','F','G','H','I','J'];

for i=1:length(C)
    for j=1:1000
        in1 = sprintf('%.3d-%c.png',j,C(i));
       
        image1 = imread(in1,'PNG')/255;
        image1c = Canvanagh(image1,n);
        image1c = image1c(1:n/2,1:n/2);
        
        fid = fopen('raw.txt', 'a+');
        fprintf(fid, '%f\t', image1);
        fprintf(fid, '%d\n', i-1);
        fclose(fid);
        
        fid = fopen('features.txt', 'a+');
        fprintf(fid, '%f\t', image1c);
        fprintf(fid, '%d\n', i-1);
        fclose(fid);
    end;
end;
