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
      text: '中文工作流',
      items: [
        { text: '工作流索引', link: '/tutorials/' },
        { text: "点数据与 Moran's I", link: '/tutorials/getting-started' },
        { text: '格式 I/O 与验证', link: '/tutorials/format-workflows' }
      ]
    },
    {
      text: '数据模型与 I/O',
      collapsed: true,
      items: [
        { text: '功能模块索引', link: '/modules/' },
        { text: '核心概念与对象契约', link: '/modules/core-concepts' },
        { text: '读取神经影像数据', link: '/modules/reading-data' },
        { text: 'Schema 验证与 manifest', link: '/modules/schema-validation' },
        { text: '互操作与可审计交换', link: '/modules/interoperability' },
        { text: '可扩展 I/O', link: '/modules/scalable-io' },
        { text: '文件后端 values', link: '/modules/file-backed-io' }
      ]
    },
    {
      text: '空间关系与 support',
      collapsed: true,
      items: [
        { text: '邻接关系与空间权重', link: '/modules/neighbors-and-weights' },
        { text: '分区与聚合', link: '/modules/parcellation-and-aggregation' },
        { text: '空间支持变换', link: '/modules/change-of-support' },
        { text: '真实数据 support mapping', link: '/modules/real-world-support-mapping' },
        { text: '显式 transform resampling', link: '/modules/transform-aware-resampling' },
        { text: '空间与 transform path', link: '/modules/space-transform-graph' }
      ]
    },
    {
      text: '推断与模型',
      collapsed: true,
      items: [
        { text: 'Support uncertainty', link: '/modules/support-uncertainty' },
        { text: 'Support-aware inference', link: '/modules/support-aware-inference' },
        { text: '有界空间建模', link: '/modules/spatial-modelling' },
        { text: '空间模型不确定性', link: '/modules/model-uncertainty' },
        { text: '迭代空间模型', link: '/modules/iterative-spatial-models' },
        { text: '时间与时空分析', link: '/modules/spatiotemporal-analysis' }
      ]
    },
    {
      text: '执行与复现',
      collapsed: true,
      items: [
        { text: '有界科学计算', link: '/modules/bounded-execution' },
        { text: '可审计 replay', link: '/modules/reproducible-replay' }
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
        { text: 'Installation and basic use', link: '/en/guide/' },
        { text: 'Module index', link: '/en/modules/' }
      ]
    },
    {
      text: 'Data model and I/O',
      items: [
        { text: 'Core concepts', link: '/en/tutorials/core-concepts' },
        { text: 'Reading data', link: '/en/tutorials/reading-data' },
        { text: 'Schema validation', link: '/en/modules/schema-validation' },
        { text: 'Interoperability', link: '/en/modules/interoperability' },
        { text: 'Scalable I/O', link: '/en/modules/scalable-io' },
        { text: 'File-backed values', link: '/en/modules/file-backed-io' }
      ]
    },
    {
      text: 'Spatial relations and support',
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
      text: 'Inference and models',
      items: [
        { text: 'Support uncertainty', link: '/en/modules/support-uncertainty' },
        { text: 'Support-aware inference', link: '/en/modules/support-aware-inference' },
        { text: 'Spatial modelling', link: '/en/tutorials/spatial-modelling' },
        { text: 'Model uncertainty', link: '/en/modules/model-uncertainty' },
        { text: 'Iterative spatial models', link: '/en/modules/iterative-spatial-models' },
        { text: 'Spatiotemporal analysis', link: '/en/modules/spatiotemporal-analysis' }
      ]
    },
    {
      text: 'Execution and reproducibility',
      items: [
        { text: 'Bounded execution', link: '/en/modules/bounded-execution' },
        { text: 'Reproducible replay', link: '/en/modules/reproducible-replay' }
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
