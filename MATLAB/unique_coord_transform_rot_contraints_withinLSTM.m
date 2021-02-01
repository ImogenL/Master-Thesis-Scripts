%Rotational constraints function at bottom of script
Transform_measure_List = zeros(6,1);
MAE_List = zeros(6,1);
MSE_List = zeros(6,1);
count = 1;
%%
%X = latent1; Y = latent2; %Progress through combinations
%X = latent1; Y = latent3;
%X = latent1; Y = latent4;
%X = latent2; Y = latent3;
%X = latent2; Y = latent4;
X = latent3; Y = latent4;

Xnorm = 2500.*X./norm(X, 'fro');
Ynorm = 2500.*Y./norm(Y, 'fro');

B = optimvar('B',4,4);
A = @(B)( (norm(B*transpose(Xnorm) - transpose(Ynorm)))^2 );

nonlcon = @rot_constraints;

B0 = rand(4,4);

options = optimoptions(@fmincon,'MaxFunctionEvaluations',6e+05,'StepTolerance',1e-09); %default 3e+03, 1e-06
sol =  fmincon(A,B0,[],[],[],[],[],[],nonlcon, options);

shouldbeclosetozero = norm(sol*transpose(Xnorm)) - norm(transpose(Xnorm));
check = transpose(sol*transpose(Xnorm)); %recalc Y

Transform_measure = (norm(sol*transpose(Xnorm) - transpose(Ynorm)))^2/(norm(Ynorm))^2;
MAE = mean(abs(sol*transpose(Xnorm) - transpose(Ynorm)),'all');
MSE = mean((sol*transpose(Xnorm) - transpose(Ynorm)).^2,'all');

Transform_measure_List(count) = Transform_measure;
MAE_List(count) = MAE;
MSE_List(count) = MSE;

count = count+1;
%%
labels = ["X = latent1 Y = latent2";"X = latent1 Y = latent3";"X = latent1 Y = latent4";"X = latent2 Y = latent3";"X = latent2 Y = latent4";"X = latent3 Y = latent4"];
T = table(labels,MAE_List,MSE_List,Transform_measure_List);
writetable(T,'table_coord_transform_withinLSTM0209.csv')

%% Function
function [c,ceq] = rot_constraints(B)
c = [];
ceq(1) = norm(pinv(B) - transpose(B)); %orthogonal
ceq(2) = det(B) - 1; %det = 1

end