import * as React from 'react'
import { NextStatelessComponent } from 'next'
import { Show } from '../lib/models/tvmaze'
import fetch from 'isomorphic-unfetch'
import Link from 'next/link'
import Layout from '../components/Layout'

interface IndexProps {
  shows: Show[]
}

interface ShowLinkProps {
  show: Show
}

const ShowLink: React.SFC<ShowLinkProps> = ({ show }) => (
  <li key={show.id}>
    <Link as={`/p/${show.id}`} href={`/post?id=${show.id}`}>
      <a>{show.name}</a>
    </Link>

    <style jsx>{`
      li {
        list-style: none;
        margin: 5px 0;
      }

      a {
        text-decoration: none;
        color: blue;
      }

      a:hover {
        opacity: 0.6;
      }
    `}</style>
  </li>
)

const Index: NextStatelessComponent<IndexProps> = (props) => (
  <Layout>
    <h1>Batman TV Shows</h1>
    <ul>
      {props.shows.map(show => (
        <ShowLink key={show.id} show={show} />
      ))}
    </ul>

    <style jsx>{`
      h1, a {
        font-family: "Arial";
      }

      ul {
        padding: 0;
      }
    `}</style>
  </Layout>
)

Index.getInitialProps = async function () {
  const res = await fetch('https://api.tvmaze.com/search/shows?q=batman')
  const data = await res.json()

  console.log(`Show data fetched. Count: ${data.length}`)

  return {
    shows: data.map(r => r.show)
  }
}


export default Index
