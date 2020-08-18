-- custom header
nvim.g.dashboard_custom_header = {
"";
"";
"";
"";
"";
"";
"";
"";
"";
"";
"";
"";
"";
"";
"";
"";
"";
"";
"";
}

-- headercolor
nvim.command('hi! dashboardHeader guifg=#c3e88d')
nvim.command('hi! dashboardCenter guifg=#89ddff')
nvim.command('hi! dashboardShortcut guifg=#c792ea')
nvim.command('hi! dashboardFooter guifg=#676E95')
-- use fzf in dashboard
nvim.g.dashboard_default_executive = "fzf"

