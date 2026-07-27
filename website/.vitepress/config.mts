import { defineConfig } from 'vitepress'

const repository = 'https://github.com/zh1peng/neurogeo'

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
      description: '从空间对象到可审计空间推断'
    },
    en: {
      label: 'English',
      lang: 'en',
      link: '/en/',
      title: 'neurogeo',
      description: 'Auditable spatial analysis for neuroimaging'
    }
  },
  themeConfig: {
    search: {
      provider: 'local'
    },
    socialLinks: [
      { icon: 'github', link: repository }
    ],
    locales: {
      root: {
        label: '简体中文',
        nav: [
          { text: '开始', link: '/guide/' },
          { text: '教程', link: '/tutorials/' },
          { text: '核心概念', link: '/concepts/' },
          { text: '函数参考', link: '/api/reference/' }
        ],
        sidebar: {
          '/guide/': [
            {
              text: '开始使用',
              items: [
                { text: '学习路线', link: '/guide/' },
                { text: '第一次完整分析', link: '/tutorials/getting-started' }
              ]
            }
          ],
          '/tutorials/': [
            {
              text: '从头完成一次分析',
              items: [
                { text: '教程导航', link: '/tutorials/' },
                { text: "空间对象到 Moran's I", link: '/tutorials/getting-started' },
                { text: '真实格式读取与验证', link: '/tutorials/format-workflows' }
              ]
            }
          ],
          '/concepts/': [
            {
              text: '理解数据再计算',
              items: [
                { text: 'NGCS 概念地图', link: '/concepts/' }
              ]
            }
          ]
        },
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
      },
      en: {
        label: 'English',
        nav: [
          { text: 'Guide', link: '/en/guide/' },
          { text: 'Tutorials', link: '/en/tutorials/' },
          { text: 'API reference', link: '/api/reference/' }
        ],
        sidebar: {
          '/en/guide/': [
            {
              text: 'Get started',
              items: [
                { text: 'Learning path', link: '/en/guide/' },
                { text: 'Core concepts', link: '/en/tutorials/core-concepts' }
              ]
            }
          ],
          '/en/tutorials/': [
            {
              text: 'Tutorials',
              items: [
                { text: 'Overview', link: '/en/tutorials/' },
                { text: 'Core concepts', link: '/en/tutorials/core-concepts' },
                { text: 'Reading data', link: '/en/tutorials/reading-data' },
                { text: 'Neighbors and weights', link: '/en/tutorials/neighbors-and-weights' },
                { text: 'Parcellation and aggregation', link: '/en/tutorials/parcellation-and-aggregation' },
                { text: 'Change of support', link: '/en/tutorials/change-of-support' },
                { text: 'Spatial modelling', link: '/en/tutorials/spatial-modelling' }
              ]
            }
          ]
        },
        outline: {
          level: [2, 3],
          label: 'On this page'
        }
      }
    }
  }
})
