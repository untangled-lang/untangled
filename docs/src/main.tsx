/*
 * This component wraps the main MDX component with no other contribution; its only purpose is to
 * create a HMR boundary so that changes to the MDX don’t cause the page to reload and jump to the
 * top.
 */
import MainMdx from './main.mdx';

export default function Main(props: Parameters<typeof MainMdx>[0]) {
  return <MainMdx {...props} />;
}
