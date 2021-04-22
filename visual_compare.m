figure
plot(resultsindiv.lifetime, resultsindiv.id, '.')
hold on
plot(resultsindiv.ul20, resultsindiv.id, '-.')
hold on
plot(resultsindiv.uu20, resultsindiv.id, '-.')
hold on
plot(resultsindiv.ul100, resultsindiv.id, '--')
hold on
plot(resultsindiv.uu100, resultsindiv.id, '--')
hold on
plot(resultsindiv.ul1000, resultsindiv.id, 'x')
hold on
plot(resultsindiv.uu1000, resultsindiv.id, 'x')
hold on
plot(resultsindiv.naivel, resultsindiv.id, ':')
hold on
plot(resultsindiv.naiveu, resultsindiv.id, ':')


%%

figure
for i = 2:101
    plot(resultsindivS3(1:10,1),resultsindivS3(1:10,i),':','Color',[0,0.1,0.1])
    hold on
end
hold on
plot(resultsindivS3(1:10,1),resultsindivS3(1:10,102),'-','Color',[0,1,0.9])
str = [repmat('',10,1) num2str(resultsindivS3(1:10,102))];
hold on
text(resultsindivS3(1:10,1),resultsindivS3(1:10,102),cellstr(str))