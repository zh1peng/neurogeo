import { defineConfig } from 'vitepress'

const repository = 'https://github.com/zh1peng/neurogeo'

const zhTheme = {
  sidebar: [
    {
      text: '概览',
      items: [
        { text: '软件包概览', link: '/' },
        { text: '安装与基本用法', link: '/guide/' },
        { text: 'NGCS 数据模型', link: '/concepts/' }
      ]
    },
    {
      text: '分析工作流',
      items: [
        { text: '工作流索引', link: '/tutorials/' },
        { text: "点数据与 Moran's I", link: '/tutorials/getting-started' },
        { text: '格式 I/O 与验证', link: '/tutorials/format-workflows' }
      ]
    },
    {
      text: '参考',
      items: [
        { text: '函数参考', link: '/api/reference/' }
      ]
    }
  ],
  outline: {
    level: [2, 3],
    label: '本页内容'
  },
  docFooter: {
    prev: '上一篇',
    next: '下一篇'
  },
  lastUpdated: {
    text: '最后更新'
  },
  editLink: {
    pattern: `${repository}/edit/main/:path`,
    text: '在 GitHub 上编辑此页'
  },
  sidebarMenuLabel: '目录',
  returnToTopLabel: '返回顶部',
  langMenuLabel: '切换语言',
  darkModeSwitchLabel: '外观'
}

const enTheme = {
  sidebar: [
    {
      text: 'Overview',
      items: [
        { text: 'Package overview', link: '/en/' },
        { text: 'Installation and basic use', link: '/en/guide/' }
      ]
    },
    {
      text: 'Data model',
      items: [
        { text: 'Core concepts', link: '/en/tutorials/core-concepts' },
        { text: 'Reading data', link: '/en/tutorials/reading-data' }
      ]
    },
    {
      text: 'Analysis workflows',
      items: [
        { text: 'Workflow index', link: '/en/tutorials/' },
        { text: 'Neighbors and weights', link: '/en/tutorials/neighbors-and-weights' },
        { text: 'Parcellation and aggregation', link: '/en/tutorials/parcellation-and-aggregation' },
        { text: 'Change of support', link: '/en/tutorials/change-of-support' },
        { text: 'Spatial modelling', link: '/en/tutorials/spatial-modelling' }
      ]
    },
    {
      text: 'Reference',
      items: [
        { text: 'Function reference', link: '/api/reference/' }
      ]
    }
  ],
  outline: {
    level: [2, 3],
    label: 'On this page'
  }
}

export default defineConfig({
  title: 'neurogeo',
  description: '神经影像空间数据与空间统计的可复现分析工具',
  base: '/neurogeo/',
  cleanUrls: true,
  lastUpdated: true,
  ignoreDeadLinks: [/^\/api\//],
  markdown: {
    math: true
  },
  head: [
    ['meta', { name: 'theme-color', content: '#176b63' }]
  ],
  locales: {
    root: {
      label: '简体中文',
      lang: 'zh-CN',
      title: 'neurogeo',
      description: 'Neuroimaging Geoinformatics Core Specification 的 R 参考实现',
      themeConfig: zhTheme
    },
    en: {
      label: 'English',
      lang: 'en',
      link: '/en/',
      title: 'neurogeo',
      description: 'R reference implementation of the Neuroimaging Geoinformatics Core Specification',
      themeConfig: enTheme
    }
  },
  themeConfig: {
    search: {
      provider: 'local'
    },
    socialLinks: [
      { icon: 'github', link: repository }
    ]
  }
})
