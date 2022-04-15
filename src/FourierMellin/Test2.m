%   Main function which runs the invariance test.
%   
%   Open image files
%   The Matlab Code for manipulating the images is a bit shitty so
%   the first image is the original the second is the transformed image.
[my_filename,my_filepath] = uigetfile( '*.bmp', 'Select file' );
filename = strcat ( my_filepath, my_filename );
%   Open and read the files
tst_image = imread(filename,'BMP');
tst_image = tst_image(:,:,1);

[my_filename,my_filepath] = uigetfile( '*.bmp', 'Select file' );
filename = strcat ( my_filepath, my_filename );
L_tst_image = imread(filename,'BMP');
L_tst_image = L_tst_image(:,:,1);

imtool(tst_image,'DisplayRange',[]);
imtool(L_tst_image,'DisplayRange',[]);

% Original Image
% magnitude of fourier representation
tst_fft = fft2(tst_image,1024,1024);
tst_fft = fftshift(tst_fft);
tst_abs_fft = abs(tst_fft);
% A bit tricky here. the dynamic range of the FFT values is too high for a
% good log polar mapping due to interpolation problems. use the logarithm
% of the abs(fft) values for the remainder of the algorithm.
% Is this correct to do before the log polar mapping, or does it remove
% the ability to differentiate between transforms?

tst_abs_fft = log(tst_abs_fft);
lp_tst_abs_fft = LogPolar2(tst_abs_fft,0);
inv_tst = fft2(lp_tst_abs_fft,1024,1024);
inv_tst = fftshift(inv_tst);
inv_tst = abs(inv_tst);
inv_tst = log(inv_tst);
imtool(inv_tst,'DisplayRange',[]);

% Test non invariant 
%tst_abs_fft = log(tst_abs_fft);
%imtool(tst_abs_fft,'DisplayRange',[]);

% Transformed Image
% magnitude of fourier representation
L_tst_fft = fft2(L_tst_image,1024,1024);
L_tst_fft = fftshift(L_tst_fft);
L_tst_abs_fft = abs(L_tst_fft);

L_tst_abs_fft = log(L_tst_abs_fft);
lp_L_tst_abs_fft = LogPolar2(L_tst_abs_fft,0);
inv_L_tst = fft2(lp_L_tst_abs_fft,1024,1024);
inv_L_tst = fftshift(inv_L_tst);
inv_L_tst = abs(inv_L_tst);
inv_L_tst = log(inv_L_tst);
imtool(inv_L_tst,'DisplayRange',[]);

% Test non invariant
%L_tst_abs_fft = log(L_tst_abs_fft);
%imtool(L_tst_abs_fft,'DisplayRange',[]);