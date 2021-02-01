%Rotational constraints function at bottom of script
X = LSTM; Y = CNN;

Xnorm = 250.*X./norm(X, 'fro');
Ynorm = 250.*Y./norm(Y, 'fro');

B = optimvar('B',4,4);
A = @(B)( (norm(B*transpose(Xnorm) - transpose(Ynorm)))^2 );

nonlcon = @rot_constraints;

B0 = rand(4,4);
options = optimoptions(@fmincon,'MaxFunctionEvaluations',6e+05,'StepTolerance',1e-09); %default 3e+03, 1e-06
sol =  fmincon(A,B0,[],[],[],[],[],[],nonlcon, options);

shouldbeclosetozero = norm(sol*transpose(Xnorm)) - norm(transpose(Xnorm)); %norm should be preserved
check = transpose(sol*transpose(Xnorm)); %recalc Y

Transform_measure = (norm(sol*transpose(Xnorm) - transpose(Ynorm)))^2/(norm(Ynorm))^2;
MAE = mean(abs(sol*transpose(Xnorm) - transpose(Ynorm)),'all');
MSE = mean((sol*transpose(Xnorm) - transpose(Ynorm)).^2,'all');


%% Function
function [c,ceq] = rot_constraints(B)
c = [];
ceq(1) = norm(pinv(B) - transpose(B)); %orthogonal
ceq(2) = det(B) - 1; %det = 1

end