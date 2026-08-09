import { defineConfig } from 'vitepress'

const repository = 'https://github.com/zh1peng/neurogeo'

const zhTheme = {
  logo: '/logo.png',
  nav: [
    { text: '开始', link: '/guide/' },
    { text: '教程', link: '/tutorials/' },
    { text: '方法与假设', link: '/modules/' },
    { text: 'API', link: '/api/reference/' }
  ],
  sidebar: [
    {
      text: '从这里开始',
      items: [
        { text: '软件包概览', link: '/' },
        { text: '安装与第一次运行', link: '/guide/' },
        { text: '安装、版本与可选后端', link: '/guide/installation' },
        { text: '15 分钟快速开始', link: '/tutorials/getting-started' },
        { text: '按数据格式选择入口', link: '/tutorials/format-workflows' },
        { text: '核心概念', link: '/concepts/' },
        { text: '运行时 options', link: '/concepts/options' },
        { text: '用户术语表', link: '/glossary/' }
      ]
    },
    {
      text: '读取、检查与写出',
      collapsed: true,
      items: [
        { text: '读取神经影像数据', link: '/modules/reading-data' },
        { text: '对象与测量语义', link: '/modules/core-concepts' },
        { text: '质量控制', link: '/modules/quality-control' },
        { text: '皮层二维地图', link: '/modules/cortical-cartography' },
        { text: '互操作与交换', link: '/modules/interoperability' },
        { text: '可扩展与文件后端 I/O', link: '/modules/scalable-io' }
      ]
    },
    {
      text: '空间关系与 support',
      collapsed: true,
      items: [
        { text: '邻接、距离与空间权重', link: '/modules/neighbors-and-weights' },
        { text: '分区与聚合', link: '/modules/parcellation-and-aggregation' },
        { text: '空间 support 转换', link: '/modules/change-of-support' },
        { text: '真实数据 support mapping', link: '/modules/real-world-support-mapping' },
        { text: '显式 transform 重采样', link: '/modules/transform-aware-resampling' },
        { text: '空间与 transform path', link: '/modules/space-transform-graph' }
      ]
    },
    {
      text: '统计、推断与模型',
      collapsed: true,
      items: [
        { text: 'Support uncertainty', link: '/modules/support-uncertainty' },
        { text: 'Support-aware inference', link: '/modules/support-aware-inference' },
        { text: '多 layer 与组水平推断', link: '/modules/group-inference' },
        { text: '空间模型', link: '/modules/spatial-modelling' },
        { text: '模型不确定性', link: '/modules/model-uncertainty' },
        { text: '时空分析', link: '/modules/spatiotemporal-analysis' },
        { text: '实验方法边界', link: '/modules/advanced-spatial-methods' }
      ]
    },
    {
      text: '验证与复现',
      collapsed: true,
      items: [
        { text: 'Schema 与 manifest', link: '/modules/schema-validation' },
        { text: '有界执行', link: '/modules/bounded-execution' },
        { text: '可审计 replay', link: '/modules/reproducible-replay' },
        { text: '全部模块索引', link: '/modules/' }
      ]
    }
  ],
  outline: { level: [2, 3], label: '本页内容' },
  docFooter: { prev: '上一篇', next: '下一篇' },
  lastUpdated: { text: '最后更新' },
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
  logo: '/logo.png',
  nav: [
    { text: 'Start', link: '/en/guide/' },
    { text: 'Tutorials', link: '/en/tutorials/' },
    { text: 'Methods & assumptions', link: '/en/modules/' },
    { text: 'API', link: '/api/reference/' }
  ],
  sidebar: [
    {
      text: 'Start here',
      items: [
        { text: 'Package overview', link: '/en/' },
        { text: 'Installation and first run', link: '/en/guide/' },
        { text: 'Installation and backends', link: '/en/guide/installation' },
        { text: '15-minute quickstart', link: '/en/tutorials/getting-started' },
        { text: 'Choose by data format', link: '/en/tutorials/format-workflows' },
        { text: 'Core concepts', link: '/en/tutorials/core-concepts' },
        { text: 'User glossary', link: '/en/glossary/' }
      ]
    },
    {
      text: 'Read inspect and write',
      collapsed: true,
      items: [
        { text: 'Reading neuroimaging data', link: '/en/tutorials/reading-data' },
        { text: 'Quality control', link: '/en/modules/quality-control' },
        { text: 'Cortical cartography', link: '/en/modules/cortical-cartography' },
        { text: 'Interoperability', link: '/en/modules/interoperability' },
        { text: 'Scalable I/O', link: '/en/modules/scalable-io' },
        { text: 'File-backed values', link: '/en/modules/file-backed-io' }
      ]
    },
    {
      text: 'Spatial relations and support',
      collapsed: true,
      items: [
        { text: 'Neighbors and weights', link: '/en/tutorials/neighbors-and-weights' },
        { text: 'Parcellation and aggregation', link: '/en/tutorials/parcellation-and-aggregation' },
        { text: 'Change of support', link: '/en/tutorials/change-of-support' },
        { text: 'Real-world support mapping', link: '/en/modules/real-world-support-mapping' },
        { text: 'Transform-aware resampling', link: '/en/modules/transform-aware-resampling' },
        { text: 'Spaces and transform paths', link: '/en/modules/space-transform-graph' }
      ]
    },
    {
      text: 'Statistics inference and models',
      collapsed: true,
      items: [
        { text: 'Support uncertainty', link: '/en/modules/support-uncertainty' },
        { text: 'Support-aware inference', link: '/en/modules/support-aware-inference' },
        { text: 'Multilayer inference', link: '/en/modules/multilayer-inference' },
        { text: 'Spatial modelling', link: '/en/tutorials/spatial-modelling' },
        { text: 'Model uncertainty', link: '/en/modules/model-uncertainty' },
        { text: 'Spatiotemporal analysis', link: '/en/modules/spatiotemporal-analysis' }
      ]
    },
    {
      text: 'Validation and reproducibility',
      collapsed: true,
      items: [
        { text: 'Schema and manifests', link: '/en/modules/schema-validation' },
        { text: 'Bounded execution', link: '/en/modules/bounded-execution' },
        { text: 'Auditable replay', link: '/en/modules/reproducible-replay' },
        { text: 'All modules', link: '/en/modules/' }
      ]
    }
  ],
  outline: { level: [2, 3], label: 'On this page' },
  editLink: {
    pattern: `${repository}/edit/main/:path`,
    text: 'Edit this page on GitHub'
  }
}

export default defineConfig({
  title: 'neurogeo',
  description: 'Auditable spatial data and statistics for neuroimaging',
  base: '/neurogeo/',
  cleanUrls: true,
  lastUpdated: true,
  ignoreDeadLinks: [/^\/api\//],
  markdown: { math: true },
  head: [
    ['meta', { name: 'theme-color', content: '#176b63' }],
    ['link', { rel: 'icon', type: 'image/png', href: '/neurogeo/favicon.png' }],
    ['link', { rel: 'apple-touch-icon', href: '/neurogeo/logo.png' }]
  ],
  locales: {
    root: {
      label: '简体中文',
      lang: 'zh-CN',
      title: 'neurogeo',
      description: '面向神经影像空间数据与空间统计的可审计 R 工具包',
      themeConfig: zhTheme
    },
    en: {
      label: 'English',
      lang: 'en',
      link: '/en/',
      title: 'neurogeo',
      description: 'Auditable spatial data and statistics for neuroimaging',
      themeConfig: enTheme
    }
  },
  themeConfig: {
    search: { provider: 'local' },
    socialLinks: [{ icon: 'github', link: repository }]
  }
})
