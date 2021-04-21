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