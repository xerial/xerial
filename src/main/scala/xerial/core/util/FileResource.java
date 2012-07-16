/*--------------------------------------------------------------------------
 *  Copyright 2007 Taro L. Saito
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *--------------------------------------------------------------------------*/
//--------------------------------------
// XerialJ
//
// FileResource.java
// Since: Jul 5, 2007 10:57:31 AM
//
// $URL:http://www.xerial.org/svn/project/XerialJ/trunk/XerialJ/src/main/java/org/xerial/util/FileResource.java $
// $Author:leo $
//--------------------------------------
package xerial.core.util;

import org.xerial.util.io.VirtualFile;

import java.io.*;
import java.lang.reflect.Modifier;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;


    /**
     * VirtualFile is a common interface to handle system files and file resources in JAR.
     *
     * System file resources have an URL prefixed with "file:".
     *   e.g., "file:/C:/Program Files/Software/classes/org/xerial/util/FileResource.java"
     * JAR file contents have an URL prefixed with "jar:file:
     *   e.g., "jar:file:/C:/Program Files/Software/something.jar!/org/xerial/util/FileResource.java"
     *
     * @author leo
     *
     */
    interface VirtualFile
    {
        /**
         * Gets the logical path of the file.
         * For example, if this VirtualFile' URL is "file:/somewhere/org/xerial/util/FileResource.java",
         * its logical name is org/xerial/util/FileResource.java, beginning from the root package.
         * @return
         */
        String getLogicalPath();

        /**
         * is directory?
         * @return true when the file is a directory, otherwise false
         */
        boolean isDirectory();

        /**
         * Gets the URL of this file
         * @return
         */
        URL getURL();

        String toString();

    }

/**
 * An interface to filter file resources
 * @author leo
 *
 */
interface ResourceFilter
{
    /**
     * Test whether the input resource is acceptable or not.
     * @param resourcePath
     * @return true if accectable, otherwise false
     */
    boolean accept(String resourcePath);
}


/**
 * Gets file URLs by using the {@link ClassLoader} facility. This utility can be
 * used to hold file resources within a package.
 * 
 * 
 * Usage: <code>
 * <pre>
 * // if YourClass belongs to a package org.xerial.util, the following code searches org/xerial/util/sample.txt in the source folder (or jar file) 
 * URL fileResourceURL = FileResource.find(YourClass.class, &quot;sample.txt&quot;);
 * 
 * // another form of the previous line
 * URL fileResourceURL2 = FileResource.find(&quot;org.xerial.util&quot;, &quot;sample.txt&quot;);
 * 
 * // short-hand code to get Reader (equivalent to call new BufferedReader(new InputStreamReader(URL#openStream())) )
 * BufferedReader reader = FileResource.open(YourClass.class, &quot;sample.txt&quot;);
 * </pre>
 * </code>
 * 
 * @author leo
 * 
 */
public class FileResource  {
    private static Logger _logger = Logger$.apply(FileResource.class);

    /**
     * A virtual file implementation for usual files
     * 
     * @author leo
     * 
     */
    public static class SystemFile implements VirtualFile {
        private File file;
        private String logicalPath;

        public SystemFile(File file, String logicalPath) {
            this.file = file;
            this.logicalPath = logicalPath;
        }

        public String getLogicalPath() {
            return logicalPath;
        }

        public URL getURL() {
            try {
                return file.toURI().toURL();
            }
            catch (MalformedURLException e) {
                throw new IllegalArgumentException(e);
            }
        }

        public boolean isDirectory() {
            return file.isDirectory();
        }

        @Override
        public String toString() {
            return getURL().toString();
        }

    }

    /**
     * A virtual file implementation for file resources contained in a JAR file
     * 
     * @author leo
     * 
     */
    public static class FileInJarArchive implements VirtualFile {
        private URL resourceURL;
        private String logicalPath;
        boolean isDirectory;

        public FileInJarArchive(URL resourceURL, String logicalPath, boolean isDirectory) {
            this.resourceURL = resourceURL;
            this.logicalPath = logicalPath;
            this.isDirectory = isDirectory;

            if (this.resourceURL == null)
                throw new IllegalArgumentException("resource URL cannot be null: " + logicalPath);
        }

        public String getLogicalPath() {
            return logicalPath;
        }

        public URL getURL() {
            return resourceURL;
        }

        public boolean isDirectory() {
            return isDirectory;
        }

        @Override
        public String toString() {
            return getURL().toString();
        }

    }

    /**
     * list up resources recursively under the given base package name
     * 
     * @param basePackageName
     *            package name to search. e.g. org.xerial.util
     * @return the list of resources represented as {@link VirtualFile}
     */
    public static List<VirtualFile> listResources(String basePackageName) {
        return listResources(basePackageName, new ResourceFilter() {
            public boolean accept(String resourcePath) {
                return true;
            }
        });
    }

    /**
     * list up resources recursively under the given base package name
     * 
     * @param basePackageName
     *            package name to search. e.g. org.xerial.util
     * @param classLoader
     *            class loader
     * @return the list of resources represented as {@link VirtualFile}
     */
    public static List<VirtualFile> listResources(String basePackageName, ClassLoader classLoader) {
        return listResources(classLoader, basePackageName, new ResourceFilter() {
            public boolean accept(String resourcePath) {
                return true;
            }
        });
    }

    /**
     * Lists up all resources satisfying the given resourceFilter from the
     * current class loader
     * 
     * @param resourceFilter
     * @return the list of resources matching the condition specified in the
     *         resourceFilter
     */
    public static List<VirtualFile> listResources(ResourceFilter resourceFilter) {
        return listResources(Thread.currentThread().getContextClassLoader(), resourceFilter);
    }

    /**
     * Lists up all resources satisfying the given resourceFilter from the given
     * current class loader
     * 
     * @param resourceFilter
     * @param classLoader
     * @return the list of resources matching the condition specified in the
     *         resourceFilter
     */
    public static List<VirtualFile> listResources(ClassLoader classLoader,
            ResourceFilter resourceFilter) {
        List<URLClassLoader> classLoaderList = getAllAncestorOrSelfClassLoaders(classLoader);
        ArrayList<VirtualFile> fileList = new ArrayList<VirtualFile>();
        for (URLClassLoader cl : classLoaderList) {
            for (URL url : cl.getURLs()) {
                fileList.addAll(listResources(url, "", resourceFilter));
            }
        }

        return fileList;
    }

    /**
     * Lists up all resources recursively under the given resourceURL. If the
     * resourceURL is file, this method searches its sub directories, when it is
     * Jar file, it searches contents of the Jar file for other resources.
     * 
     * @param resourceURL
     * @param resourceFilter
     * @return the list of resources matching the given resource filter
     */
    public static List<VirtualFile> listResources(URL resourceURL, String packagePath,
            ResourceFilter resourceFilter) {
        _logger.trace("listResource: url=" + resourceURL);

        ArrayList<VirtualFile> fileList = new ArrayList<VirtualFile>();
        if (resourceURL == null)
            return fileList;

        String protocol = resourceURL.getProtocol();
        if (protocol.equals("file")) {
            String resourceURLString = resourceURL.toString();
            collectFileResources(resourceURLString, fileList, packagePath, resourceFilter);
        }
        else if (protocol.equals("jar")) {
            // retrieve jar contents
            String path = resourceURL.getPath();
            int pos = path.indexOf("!");
            if (pos < 0)
                throw new IllegalArgumentException("invalid resource URL: " + resourceURL);

            String jarPath = path.substring(0, pos);
            String filePath = path.substring(pos + 2);
            try {
                String jarURLString = "jar:" + jarPath;

                jarPath = jarPath.replaceAll("%20", " ").replace("file:", "");
                filePath = filePath.replaceAll("%20", " ");

                // File jarFile = new File(jarPath);
                JarFile jf = new JarFile(jarPath);
                for (Enumeration<JarEntry> entryEnum = jf.entries(); entryEnum.hasMoreElements();) {
                    JarEntry jarEntry = entryEnum.nextElement();

                    String physicalURL = jarURLString + "!/" + jarEntry.getName();
                    URL jarFileURL = new URL(physicalURL);

                    String logicalName = extractLogicalName(packagePath, jarEntry.getName());
                    if (logicalName != null && resourceFilter.accept(logicalName))
                        fileList.add(new FileInJarArchive(jarFileURL, logicalName, jarEntry
                                .isDirectory()));
                }

            }
            catch (MalformedURLException e) {
                _logger.error(e);
            }
            catch (IOException e) {
                _logger.error(e);
            }

        }
        else {
            throw new UnsupportedOperationException(
                    "resources other than file or jar are not supported: " + resourceURL);
        }

        return fileList;
    }

    /**
     * list up resources recursively under the given base package name
     * 
     * @param basePackageName
     *            package name to search. e.g. org.xerial.util
     * @return the list of resources represented as {@link VirtualFile}
     */
    public static List<VirtualFile> listResources(String basePackageName,
            ResourceFilter resourceFilter) {
        return listResources(Thread.currentThread().getContextClassLoader(), basePackageName,
                resourceFilter);
    }

    /**
     * list up resources recursively under the given base package name
     * 
     * @param classLoader
     * @param basePackageName
     *            package name to search. e.g. org.xerial.util
     * @return the list of resources represented as {@link VirtualFile}
     */
    public static List<VirtualFile> listResources(ClassLoader classLoader, String basePackageName,
            ResourceFilter resourceFilter) {
        String packagePath = packagePath(basePackageName);
        List<URL> resourceURLList = getURLListFromAllAncestorOrSelfClassLoaders(classLoader,
                packagePath);
        ArrayList<VirtualFile> fileList = new ArrayList<VirtualFile>();
        for (URL resourceURL : resourceURLList) {
            fileList.addAll(listResources(resourceURL, packagePath, resourceFilter));
        }

        return fileList;
    }

    private static void collectFileResources(String resourceURLString,
            ArrayList<VirtualFile> fileList, String packagePath, ResourceFilter resourceFilter) {
        if (resourceURLString.endsWith(".svn")) {
            return; // omit the .svn folder
        }

        String logicalName = extractLogicalName(packagePath, resourceURLString);
        if (logicalName == null)
            throw new IllegalArgumentException("packagePath=" + packagePath + ", resourceURL="
                    + resourceURLString);

        try {
            File file = new File(new URL(resourceURLString).toURI());
            if (resourceFilter.accept(file.getPath()))
                fileList.add(new SystemFile(file, logicalName));
            if (file.isDirectory()) {
                for (File childFile : file.listFiles()) {
                    String childResourceURL = resourceURLString
                            + (resourceURLString.endsWith("/") ? "" : "/") + childFile.getName();
                    collectFileResources(childResourceURL, fileList, packagePath, resourceFilter);
                }
            }
        }
        catch (MalformedURLException e) {
            _logger.error(e);
        }
        catch (URISyntaxException e) {
            _logger.error(e);
        }
    }

    private static String extractLogicalName(String packagePath, String resourcePath) {
        if (!packagePath.endsWith("/"))
            packagePath = packagePath + "/";

        int pos = resourcePath.indexOf(packagePath);
        if (pos < 0)
            return null;

        String logicalName = resourcePath.substring(pos + packagePath.length());
        return logicalName;
    }

    /**
     * Retrieves all {@link java.net.URLClassLoader}s that can be found from this class
     *
     * @return the list of {@link java.net.URLClassLoader}s
     */
    public static ArrayList<URLClassLoader> getAllAncestorOrSelfClassLoaders(ClassLoader classLoader) {
        ArrayList<URLClassLoader> classLoaderSet = new ArrayList<URLClassLoader>();
        for (ClassLoader clCursor = classLoader; clCursor != null; clCursor = clCursor.getParent()) {
            if (URLClassLoader.class.isInstance(clCursor)) {
                URLClassLoader urlClassLoader = (URLClassLoader) clCursor;
                classLoaderSet.add(urlClassLoader);
            }
        }
        return classLoaderSet;
    }

    private static ArrayList<URL> getURLListFromAllAncestorOrSelfClassLoaders(
            ClassLoader classLoader, String packageAsPath) {
        String path = !packageAsPath.endsWith("/") ? packageAsPath + "/" : packageAsPath;

        ArrayList<URLClassLoader> classLoaderSet = getAllAncestorOrSelfClassLoaders(classLoader);

        ArrayList<URL> resultURLList = new ArrayList<URL>();
        try {
            for (URLClassLoader loader : classLoaderSet) {
                for (Enumeration<URL> resourceEnum = loader.findResources(path); resourceEnum
                        .hasMoreElements();) {
                    URL url = resourceEnum.nextElement();
                    resultURLList.add(url);
                }
            }
        }
        catch (IOException e) {
            _logger.error(e);
        }
        return resultURLList;
    }

    private static String packagePath(Class< ? > referenceClass) {
        return packagePath(referenceClass.getPackage());
    }

    private static String packagePath(Package basePackage) {
        return packagePath(basePackage.getName());
    }

    private static String packagePath(String packageName) {
        String packageAsPath = packageName.replaceAll("\\.", "/");
        return packageAsPath.endsWith("/") ? packageAsPath : packageAsPath + "/";
    }

    /**
     * Opens a stream to a file resource that belongs to the same package to the
     * specified referenceClass
     *
     * @param referenceClass
     *            the reference class
     * @param resouceFileName
     *            the file name of the resource, relative to the package of the
     *            referenceClass
     * @return {@link java.io.BufferedReader} of the resource
     * @throws java.io.IOException
     *             when failed to open the file resource
     */
    public static BufferedReader open(Class< ? > referenceClass, String resouceFileName)
            throws IOException {
        URL url = find(referenceClass, resouceFileName);
        if (url != null) {
            return new BufferedReader(new InputStreamReader(url.openStream()));
        }
        else
            return null;
    }

    public static BufferedReader open(Class< ? > referenceClass, String resouceFileName,
            String encoding) throws IOException {
        URL url = find(referenceClass, resouceFileName);
        if (url != null) {
            return new BufferedReader(new InputStreamReader(url.openStream(), encoding));
        }
        else
            return null;
    }

    /**
     * Opens a stream to a file resource that belongs to the specified package
     *
     * @param basePackage
     *            the base package to load resources
     * @param resourceFileName
     *            the file name of the resource, relative to the package
     * @return {@link java.io.BufferedReader} of the resource
     * @throws java.io.IOException
     *             when failed to open the file resource
     */
    public static BufferedReader open(Package basePackage, String resourceFileName)
            throws IOException {
        URL url = find(basePackage, resourceFileName);
        if (url != null) {
            return new BufferedReader(new InputStreamReader(url.openStream()));
        }
        else
            return null;
    }

    public static BufferedInputStream openByteStream(Class< ? > referenceClass,
            String resourceFileName) throws IOException {
        URL url = find(referenceClass, resourceFileName);
        if (url != null) {
            return new BufferedInputStream(url.openStream());
        }
        else
            return null;
    }

    /**
     * Open the resource using BufferedInputStream
     *
     * @param basePackage
     * @param resourceFileName
     * @return
     * @throws java.io.IOException
     */
    public static BufferedInputStream openByteStream(Package basePackage, String resourceFileName)
            throws IOException {
        URL url = find(basePackage, resourceFileName);
        if (url != null) {
            return new BufferedInputStream(url.openStream());
        }
        else
            return null;
    }

    /**
     * Gets the {@link java.net.URL} of the file resource
     *
     * @param referenceClass
     *            the base class to find resources
     * @param resourceFileName
     *            the resource file name relative to the package of the
     *            referenceClass
     * @return the URL of the file resource
     */
    public static URL find(Class< ? > referenceClass, String resourceFileName) {
        return find(referenceClass.getPackage(), resourceFileName);
    }

    /**
     * Finds the {@link java.net.URL} of the resource
     *
     * @param basePackage
     *            the base package to find the resource
     * @param resourceFileName
     *            the resource file name relative to the package folder
     * @return the URL of the specified resource
     */
    public static URL find(Package basePackage, String resourceFileName) {
        return find(basePackage.getName(), resourceFileName);
    }

    /**
     * Finds the {@link java.net.URL} of the resource
     *
     * @param packageName
     *            the base package name to find the resource
     * @param resourceFileName
     *            the resource file name relative to the package folder
     * @return the URL of the specified resource
     */
    public static URL find(String packageName, String resourceFileName) {
        String packagePath = packagePath(packageName);
        String resourcePath = packagePath + resourceFileName;
        if (!resourcePath.startsWith("/"))
            resourcePath = "/" + resourcePath;
        _logger.trace("search resource: " + resourcePath);

        for (ClassLoader cl = Thread.currentThread().getContextClassLoader(); cl != null; cl = cl
                .getParent()) {
            URL r = cl.getResource(resourcePath);
            if (r != null)
                return r;
        }
        return FileResource.class.getResource(resourcePath);
    }

    /*
     * public static ArrayList<URL> listResourcesInJar(URL jarURL) { if
     * (!jarURL.getProtocol().equals("file")) throw new
     * IllegalArgumentException("not found the jar: " + jarURL);
     *
     * String jarURLString = "jar:" + jarPath;
     *
     * File jarFile = new File(jarURL.toString()); JarFile jf = new
     * JarFile(jarFile); for (Enumeration<JarEntry> entryEnum = jf.entries();
     * entryEnum.hasMoreElements();) { JarEntry jarEntry =
     * entryEnum.nextElement(); jarEntry.isDirectory();
     *
     * String physicalURL = jarURLString + "!/" + jarEntry.getName(); } }
     */

    public static URL findFromJAR(String jarPath, String filePath) {
        try {
            URL jarURL = new URL(jarPath);
            if (!jarURL.getProtocol().equals("file"))
                throw new IllegalArgumentException("not found the jar: " + jarURL);

            // String jarURLString = "jar:" + jarPath;

            File jarFile = new File(jarURL.toString());
            JarFile jf = new JarFile(jarFile);
            for (Enumeration<JarEntry> entryEnum = jf.entries(); entryEnum.hasMoreElements();) {
                JarEntry jarEntry = entryEnum.nextElement();
                jarEntry.isDirectory();

                // String physicalURL = jarURLString + "!/" +
                // jarEntry.getName();
            }
        }
        catch (MalformedURLException e) {
            _logger.error(e);
            throw new IllegalArgumentException("cannot find the jar: " + jarPath);
        }
        catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return null;

    }

    /**
     * Finds the {@link java.net.URL} of the resource
     *
     * @param resourceFileName
     *            the resource file name, relative to the root package
     * @return the URL of the specified resource
     */
    public static URL find(String resourceFileName) {
        return find("", resourceFileName);
    }

    @SuppressWarnings("unchecked")
    public static <T> List<Class<T>> findClasses(Package searchPath, Class<T> toSearch,
            ClassLoader classLoader) {
        List<Class<T>> result = new ArrayList<Class<T>>();

        String packageName = searchPath.getName();
        List<VirtualFile> classFileList = FileResource.listResources(classLoader, packageName,
                new ResourceFilter() {
                    public boolean accept(String resourcePath) {
                        return resourcePath.endsWith(".class");
                    }
                });

        for (VirtualFile vf : classFileList) {
            String logicalPath = vf.getLogicalPath();
            int dot = logicalPath.lastIndexOf(".");
            if (dot <= 0)
                continue;
            String className = packageName + "."
                    + logicalPath.substring(0, dot).replaceAll("/", ".");
            try {
                Class< ? > c = Class.forName(className, false, classLoader);
                if (!Modifier.isAbstract(c.getModifiers()) && toSearch.isAssignableFrom(c)) {
                    // found a target class
                    result.add((Class<T>) c);
                }
            }
            catch (ClassNotFoundException e) {
                continue;
            }
        }

        return result;

    }


}
