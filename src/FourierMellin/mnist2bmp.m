for i=0:9 % iterate through all digits
    file = sprintf('digit%d.mat',i);
    load(file);
    for j=1:500 %length(D) % take the first 200 images
        image = reshape(D(j,:),28,28)';
        out  = sprintf('vanilla/digit-%.2d-%.4d.png',i,j);
        imwrite(image/255,out,'PNG','bitdepth',8);
    end;
end;
