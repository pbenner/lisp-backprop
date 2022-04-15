% Show the log polar mapping
[my_filename,my_filepath] = uigetfile( '*.bmp', 'Select file' );
filename = strcat ( my_filepath, my_filename );
%   Open and read the files
tst_image = imread(filename,'BMP');
tst_image = tst_image(:,:,1);
LogPolar2(tst_image,1);