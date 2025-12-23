return {
    --
    -- REQUIREMENTS: 
    --  * NeoVim 0.11+   (this sample config will not work under lower version of NeoVim)
    --  * lspkind.nvim   (https://github.com/onsails/lspkind.nvim/)
    --
    -- save this file as opl.lua under your ~/.config/nvim/lsp directory
    -- additionally, add this entry to ~/.config/nvim/init.lua
    --
    -- vim.lsp.enable('opl')
    --
    -- set this to the local path for your Oplls binary
    cmd = { '/path/to/Oplls' },
    -- define the filetypes you want to associate with Oplls
    filetypes = { 'opl' },
    -- the below configuration options provide icons for the various completion item 
    -- kinds and requires the lspkind plugin to be added to NeoVim under your
    -- runtime path ($NVIM_INSTALLATION/share/nvim/runtime/lua/lspkind/init.lua) 
    require('lspkind').init({
    mode = 'symbol_text',
    preset = 'default',
    symbol_map = {
      Text = "󰉿",
      Method = "󰆧",
      Function = "󰊕",
      Constructor = "",
      Field = "󰜢",
      Variable = "󰀫",
      Class = "󰠱",
      Interface = "",
      Module = "",
      Property = "󰜢",
      Unit = "󰑭",
      Value = "󰎠",
      Enum = "",
      Keyword = "󰌋",
      Snippet = "",
      Color = "󰏘",
      File = "󰈙",
      Reference = "󰈇",
      Folder = "󰉋",
      EnumMember = "",
      Constant = "󰏿",
      Struct = "󰙅",
      Event = "",
      Operator = "󰆕",
      TypeParameter = "",
    },
  })
  -- end of configuration options for lspkind.nvim
}