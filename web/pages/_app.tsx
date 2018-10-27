import App, { Container } from 'next/app'
import { ApolloProvider } from 'react-apollo'
import withApollo, { ApolloClientProps } from '../lib/withApollo'

class MyApp extends App<ApolloClientProps> {
  render() {
    const { Component, pageProps, apollo } = this.props;

    return (
      <Container>
        <ApolloProvider client={apollo}>
          <Component {...pageProps} />
        </ApolloProvider>
      </Container>
    );
  }
}

export default withApollo(MyApp)
