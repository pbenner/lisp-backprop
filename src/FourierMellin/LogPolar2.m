% Convert input_image to Log Polar coordinates (backward mapping).
% Optional show_flag =1 plots the resulting image.
function lp_output = LogPolar2(input_image,show_flag)

size_u = size(input_image,2);
size_v = size(input_image,1);
[uu vv] = meshgrid(1:1:size_u,1:1:size_v);

size_lr = size_u;
size_theta = size_v;
lp_output = zeros(size_lr,size_theta);

tmpu =  zeros(size_lr,size_theta);
tmpv =  zeros(size_lr,size_theta);
% The range of theta always between -pi to pi/2
dtheta = 2*pi / size_theta;
% Get the maximum radius possible from the input image
% range from log(r=1) to log(r=rmax)
rmax = sqrt( ( (size_u+1)/2 )^2 + ( (size_v+1)/2 )^2 );
dr = (log(rmax) / size_lr);
for i = 1:size_theta               % for all possible polar coordinates
    for j = 1:size_lr              % r and theta;
        r = exp(dr*j);             % compute the corresponding cartesian
        theta = i * dtheta;        % coordinate;
        tmpu(i,j) = r * cos(theta) + (size_u+1)/2; % save coordinates in
        tmpv(i,j) = r * sin(theta) + (size_v+1)/2; % tmpu and tmpv;
    end
end

lp_output = interp2(uu,vv,input_image,tmpv,tmpu,'cubic');

% Replace the NaN elements with 0 values
ttmp=isnan(lp_output);
lp_output(ttmp)=0;

if show_flag == 1
    imtool(lp_output,'DisplayRange',[]);
end

return
